{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Network.Server
      ( Server
      , mkServer
      , appSrv
      , defaultPort
      ) where

import           Imj.Prelude

import           Control.Concurrent(threadDelay, forkIO)
import           Control.Monad.IO.Unlift(MonadUnliftIO)
import           Control.Monad.Reader(runReaderT, lift, asks)
import           Control.Monad.State.Strict(StateT, runStateT, execStateT, modify', get, gets, state)
import           Data.Char (toLower)
import qualified Data.Map.Strict as Map
import qualified Data.List as List(intercalate)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text(pack, unpack)
import qualified Data.Text as Text(intercalate)
import           Data.Tuple(swap)
import           Network.WebSockets
                  (PendingConnection, Connection,
                  -- the functions on the mext line throw(IO), hence we catch exceptions
                  -- to remove the client from the map when needed.
                   acceptRequest, sendBinaryData, sendPing, receiveData)
import           UnliftIO.Exception (SomeException(..), try)
import           UnliftIO.MVar (MVar
                                , modifyMVar_, modifyMVar, swapMVar
                                , tryReadMVar, tryTakeMVar, putMVar)

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.ClientId
import           Imj.Game.Hamazed.Network.Creation
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Notify
import           Imj.Game.Hamazed.Network.Scheduler
import           Imj.Game.Hamazed.Network.Setup
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Color.Types
import           Imj.Server.Internal.Types

import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.Text.ColorString(colored)
import           Imj.Server
import           Imj.Server.Log
import           Imj.Server.Connection

defaultPort :: ServerPort
defaultPort = ServerPort 10052

appSrv :: MVar (ServerState HamazedServerState) -> PendingConnection -> IO ()
appSrv st pending =
  acceptRequest pending >>= appSrv' st

-- We handle all "normal" exceptions (which happen due to network failures or players disconnections)
-- inside so that a broken connection of another client while broadcasting a message doesn't impact this client.
appSrv' :: MVar (ServerState HamazedServerState) -> Connection -> IO ()
appSrv' st conn =
  -- Currently, we cannot have a 'Client' in a disconnected state, this is why
  -- we do a special case for Connect. But ideally we should be able to disconnect a client.
  -- and reconnect it in the loop. To do that, when we disconnect a client, it will not
  -- be removed from the map, instead its state will be Disconnected. (TODO)
  (receiveData conn :: IO (ClientEvent HamazedServerState)) >>= \case
    Connect sn@(SuggestedPlayerName suggestedName) cliType -> either
      (\txt ->
          let response = ConnectionRefused $ pack suggestedName <> pack " is invalid:" <> txt :: ServerEvent HamazedServerState
          in sendBinaryData conn response )
      (\_ -> modifyMVar st (fmap swap . runStateT takeShipId) >>= \(cid,lifecycle) ->
               runReaderT (handleClient sn cliType lifecycle st) (ConstClient conn cid))
      $ checkName $ PlayerName $ pack suggestedName
    msg -> error $ "First sent message should be 'Connect'. " ++ show msg

handleClient :: SuggestedPlayerName -> ServerOwnership -> ClientLifecycle -> MVar (ServerState HamazedServerState) -> ReaderT ConstClient IO ()
handleClient sn cliType lifecycle st = do
  i <- asks clientId
  let disconnectOnException :: (MonadUnliftIO m) => Text -> m () -> m ()
      disconnectOnException name action = try action >>= either
        (\(e :: SomeException) ->
            liftIO $ modifyMVar_ st $ execStateT $
              onBrokenClient name (Nothing :: Maybe (Text, Text, [Text])) e i)
        return
  conn <- asks connection
  -- To detect disconnections when communication is idle:
  void $ liftIO $ forkIO $ disconnectOnException "PingPong" $ pingPong conn $ fromSecs 1
  disconnectOnException "Handler" $ do
    modifyMVar_ st $ execStateT $ do
      addClient sn cliType
      case lifecycle of
        NewClient ->
          log "Is a new client"
        ReconnectingClient gameConnectedPlayers -> do
          log "Is a reconnecting client"
          gets' worldCreation >>= \(WorldCreation creationSt wid _ _) -> case creationSt of
            Created ->
              gets clientsMap >>= notifyN [WorldRequest wid GetGameState] . Map.take 1 . flip Map.restrictKeys gameConnectedPlayers
            CreationAssigned _ ->
              -- A game is in progress and a "next level" is being built:
              -- add the newcomer to the assigned builders.
              participateToWorldCreation wid
    forever $ liftIO (receiveData conn) >>=
      modifyMVar_ st . execStateT . handleIncomingEvent

pingPong :: Connection -> Time Duration System -> IO ()
pingPong conn dt =
  go 0
 where
  go :: Int -> IO ()
  go i = do
    threadDelay $ fromIntegral $ toMicros dt
    sendPing conn $ pack $ show i
    go $ succ i

data ClientLifecycle =
    NewClient
  | ReconnectingClient !(Set ClientId)
  deriving(Show)


takeShipId :: StateT (ServerState HamazedServerState) IO (ClientId, ClientLifecycle)
takeShipId =
  takeDisconnectedShipId >>= maybe
    (state $ \s ->
      let (clients,newShipId) = takeClientId $ getClients s
      in ( (newShipId, NewClient)
         , s { getClients = clients } ))
    (\(gameConnectedPlayers, disconnected) -> do
      when (Set.null gameConnectedPlayers) $
        serverError "the current game has no connected player" -- TODO support that
      return (disconnected, ReconnectingClient gameConnectedPlayers))
 where
  takeDisconnectedShipId :: StateT (ServerState HamazedServerState) IO (Maybe (Set ClientId, ClientId)) -- (connected, disconnected)
  takeDisconnectedShipId =
    get >>= \(ServerState _ _ terminate (HamazedServerState _ _ _ _ _ _ game)) ->
      if terminate
        then
          return Nothing
        else
          liftIO (tryReadMVar game) >>= maybe
            (do serverLog $ pure "No game is running"
                return Nothing)
            (\(CurrentGame _ gamePlayers status _) -> case status of
                (Paused _ _) -> do
                -- we /could/ use 'Paused' ignored argument but it's probably out-of-date
                -- - the game scheduler thread updates it every second only -
                -- so we recompute the difference:
                  connectedPlayers <- gets onlyPlayersMap
                  let connectedPlayerKeys = Map.keysSet connectedPlayers
                      mayDisconnectedPlayerKey = Set.lookupMin $ Set.difference gamePlayers connectedPlayerKeys
                      gameConnectedPlayers = Map.keysSet $ Map.restrictKeys connectedPlayers gamePlayers
                  maybe
                    (do serverLog $ pure "A paused game exists, but has no disconnected player."
                        return Nothing)
                    (\disconnected -> do
                        serverLog $ (\i -> "A paused game exists, " <> i <> " is disconnected.") <$> showId disconnected
                        return $ Just (gameConnectedPlayers, disconnected))
                    mayDisconnectedPlayerKey
                _ -> do serverLog $ pure "A game is in progress."
                        return Nothing)

addClient :: SuggestedPlayerName -> ServerOwnership -> ClientHandlerIO HamazedServerState ()
addClient sn cliType = do
  conn <- asks connection
  i <- asks clientId
  playerColor <- mkClientColorFromCenter i <$> gets' centerColor
  presentClients <- do
    name <- makePlayerName sn
    let c = mkHamazedClient name playerColor conn cliType
    -- this call is /before/ addClient to avoid sending redundant info to client.
    notifyEveryoneN $
      map (RunCommand i)
        [ AssignName name
        , AssignColor playerColor]
    -- order matters, see comment above.
    modify' $ \ s ->
      let clients = getClients s
      in s { getClients =
              clients { getClients' =
                Map.insert i c $ getClients' clients } }
    serverLog $ (\strId -> colored "Add client" green <> "|" <> strId <> "|" <> showClient c) <$> showId i

    Map.map
      ((\cl -> PlayerEssence (getName cl) Present $ getColor cl) . unClient)
      <$> gets clientsMap
  gets' worldParameters >>= \wp ->
    notifyClientN'
      [ ConnectionAccepted i
      , ServerAppEvt $ OnWorldParameters wp
      , ServerAppEvt $ MeetThePlayers presentClients]


gameError :: String -> ClientHandlerIO HamazedServerState ()
gameError = error' "Game"

handlerError :: String -> ClientHandlerIO HamazedServerState ()
handlerError = error' "Handler"

error' :: String -> String -> ClientHandlerIO HamazedServerState ()
error' from msg = do
  log $ colored (pack txt) red
  notifyClient' $ ServerError txt
  error txt
 where
  txt = List.intercalate "|" [from, "error from Server", msg]

handleIncomingEvent :: ClientEvent HamazedServerState -> ClientHandlerIO HamazedServerState ()
handleIncomingEvent =
  withArgLogged handleIncomingEvent'

handleIncomingEvent' :: ClientEvent HamazedServerState -> ClientHandlerIO HamazedServerState ()
handleIncomingEvent' = \case
  Connect (SuggestedPlayerName sn) _ ->
    handlerError $ "already connected : " <> sn
  ClientAppEvt e -> handleIncomingEvent'' e

handleIncomingEvent'' :: ClientEventT HamazedServerState -> ClientHandlerIO HamazedServerState ()
handleIncomingEvent'' = \case
  RequestApproval cmd@(AssignName name) -> either
    (notifyClient . CommandError cmd)
    (\_ -> checkNameAvailability name >>= either
      (notifyClient . CommandError cmd)
      (\_ -> do
        adjustClient $ \c -> c { getName = name }
        acceptCmd cmd)
    ) $ checkName name
  RequestApproval cmd@(AssignColor _) ->
    notifyClient $ CommandError cmd "use Do PutColorSchemeCenter instead"
  RequestApproval cmd@(Says _) ->
    acceptCmd cmd
  RequestApproval (Leaves _) ->
    asks clientId >>= disconnect ClientShutdown -- will do the corresponding 'notifyEveryone $ RunCommand'
  Do cmd -> do
   case cmd of
    Put (ColorSchemeCenter color) -> do
      modify' $ mapState $ \s -> s { centerColor = color }
      adjustAllWithKey $ \i c -> c { getColor = mkClientColorFromCenter i color }
      gets clientsMap >>=
        notifyEveryoneN .
          map (\(k, c) -> RunCommand k (AssignColor $ getColor $ unClient c)) .
          Map.assocs
    Put (WorldShape s) ->
      onChangeWorldParams $ changeWorldShape s
    Succ x -> onDelta 1    x
    Pred x -> onDelta (-1) x
   publish $ Done cmd

  Report (Get ColorSchemeCenterKey) ->
    gets' centerColor >>= notifyClient . Reporting . Put . ColorSchemeCenter
  Report (Get WorldShapeKey) ->
    worldShape <$> gets' worldParameters >>= notifyClient . Reporting . Put . WorldShape

  ExitedState Excluded -> gets' intent >>= \case
    IntentSetup -> do
      -- change client state to make it playable
      adjustClient $ \c -> c { getState = Just ReadyToPlay }
      publish Joins
      -- The number of players just changed, so we need a new world.
      requestWorld
      -- next step when client 'IsReady'
    IntentPlayGame _ -> do
      notifyClient $ EnterState Excluded
      publish WaitsToJoin
  ExitedState Setup -> gets' intent >>= \case
    IntentSetup -> do
      modify' $ mapState $ \s -> s { intent = IntentPlayGame Nothing }
      publish StartsGame
      notifyPlayers $ ExitState Setup
      -- we need a new world so that the game starts on a world
      -- that players didn't have time to visually analyze yet.
      requestWorld
    IntentPlayGame _ -> return ()
  ExitedState (PlayLevel _) ->
    return ()

  LevelEnded outcome -> do
    adjustClient $ \c -> c { getState = Just $ Playing $ Just outcome }
    gets' intent >>= \case
      IntentPlayGame Nothing ->
        modify' $ mapState $ \s -> s { intent = IntentPlayGame $ Just outcome }
      IntentPlayGame (Just candidate) -> -- replace this by using the outcome in client state?
        case candidate of
          Won -> when (outcome /= candidate) $
            gameError $ "inconsistent outcomes:" ++ show (candidate, outcome)
          Lost _ -> case outcome of
            Won ->  gameError $ "inconsistent outcomes:" ++ show (candidate, outcome)
            Lost _ -> return () -- we allow losing reasons to differ
      IntentSetup ->
        gameError "LevelEnded received while in IntentSetup"
    gets onlyPlayersMap >>= \players' -> do
      let playing Nothing = error "should not happen"
          playing (Just ReadyToPlay) = error "should not happen"
          playing (Just (Playing Nothing)) = True
          playing (Just (Playing (Just _))) = False
          (playersPlaying, playersDonePlaying) = Map.partition (playing . getState . unClient) players'
          gameStatus =
            if null playersPlaying
              then
                WhenAllPressedAKey (OutcomeValidated outcome) (Just 2) $ Map.map (const False) playersDonePlaying
              else
                WaitingForOthersToEndLevel $ Map.keysSet playersPlaying
      notifyN [EnterState $ PlayLevel gameStatus] playersDonePlaying
      gets' scheduledGame >>= \g -> liftIO (tryTakeMVar g) >>= maybe
        (warning "LevelEnded sent while game is Nothing")
        (\game -> void $ liftIO $ putMVar g $! game { status' = gameStatus })
  CanContinue next ->
    gets' scheduledGame >>= \g -> liftIO (tryReadMVar g) >>= maybe
      (warning "CanContinue sent while game is Nothing")
      (\game -> do
          let curStatus = status' game
          case curStatus of
            WhenAllPressedAKey x Nothing havePressed -> do
              when (x /= next) $ error $ "inconsistent:"  ++ show (x,next)
              i <- lift $ asks clientId
              let newHavePressed = Map.insert i True havePressed
                  intermediateStatus = WhenAllPressedAKey x Nothing newHavePressed
              liftIO $ void $ swapMVar g $! game { status' = intermediateStatus }
              -- update to avoid state where the map is equal to all players:
              void $ updateCurrentStatus $ Just curStatus
            _ -> error $ "inconsistent:"  ++ show curStatus)

  WorldProposal wid mkEssenceRes stats -> case mkEssenceRes of
    Impossible errs -> gets' intent >>= \case
      IntentSetup -> gets' levelSpecification >>= notifyPlayers . GameInfo . CannotCreateLevel errs . levelNumber
      IntentPlayGame _ ->
        fmap levelNumber (gets' levelSpecification) >>= \ln -> do
          notifyPlayers $ GameInfo $ CannotCreateLevel errs ln
          -- TODO before launching the game we should check that it is possible to create all levels.
          serverError $ "Could not create level " ++ show ln ++ ":" ++ unpack (Text.intercalate "\n" errs)

    NeedMoreTime -> addStats stats wid
    Success essence -> gets unServerState >>= \(HamazedServerState _ levelSpec _ (WorldCreation st key spec prevStats) _ _ _) -> bool
      (serverLog $ pure $ colored ("Dropped obsolete world " <> pack (show wid)) blue)
      (case st of
        Created ->
          serverLog $ pure $ colored ("Dropped already created world " <> pack (show wid)) blue
        CreationAssigned _ -> do
          -- This is the first valid world essence, so we can cancel the request
          cancelWorldRequest
          let !newStats = safeMerge mergeStats prevStats stats
          log $ colored (pack $ show newStats) white
          modify' $ mapState $ \s -> s { worldCreation = WorldCreation Created key spec newStats }
          notifyPlayers $ ChangeLevel (mkLevelEssence levelSpec) essence key)
      $ key == wid
  CurrentGameState wid mayGameStateEssence -> maybe
    (handlerError $ "Could not get GameState " ++ show wid)
    (\gameStateEssence ->
      gets' scheduledGame >>= liftIO . tryReadMVar >>= \case
        Just (CurrentGame _ gamePlayers (Paused _ _) _) -> do
          disconnectedPlayerKeys <- Set.difference gamePlayers . Map.keysSet <$> gets onlyPlayersMap
          flip Map.restrictKeys disconnectedPlayerKeys <$> gets clientsMap >>=
            notifyN [PutGameState gameStateEssence wid]
        invalid -> handlerError $ "CurrentGameState sent while game is " ++ show invalid)
    mayGameStateEssence

  IsReady wid -> do
    adjustClient $ \c -> c { getCurrentWorld = Just wid }
    gets' intent >>= \case
      IntentSetup ->
        -- (follow-up from 'ExitedState Excluded')
        -- Allow the client to setup the world, now that the world contains its ship.
        notifyClient $ EnterState Setup
      IntentPlayGame maybeOutcome ->
        gets unServerState >>= \(HamazedServerState _ _ _ (WorldCreation _ lastWId _ _) _ _ game) ->
         liftIO (tryReadMVar game) >>= maybe
          (do
            adjustClient $ \c -> c { getState = Just ReadyToPlay }
            -- start the game when all players have the right world
            players <- gets onlyPlayersMap
            let playersAllReady =
                  all ((== Just lastWId) . getCurrentWorld . unClient) players
            when playersAllReady $ do
              adjustAll $ \c ->
                case getState c of
                  Just ReadyToPlay ->
                    c { getState = Just $ Playing Nothing
                      , getShipAcceleration = zeroCoords }
                  _ -> c
              -- 'putMVar' is non blocking because all game changes are done inside a modifyMVar
              -- of 'ServerState' and we are inside one.
              void $ liftIO $ putMVar game $ mkCurrentGame lastWId $ Map.keysSet players)
          -- a game is in progress (reconnection scenario) :
          (\(CurrentGame wid' _ _ _) -> do
              when (wid' /= lastWId) $
                handlerError $ "reconnection failed " ++ show (wid', lastWId)
              -- make player join the current game, but do /not/ set getShipSafeUntil,
              -- else disconnecting / reconnecting intentionally could be a way to cheat
              -- by having more safe time.
              adjustClient $ \c -> c { getState = Just $ Playing maybeOutcome
                                     , getShipAcceleration = zeroCoords })

  -- Due to network latency, laser shots may be applied to a world state
  -- different from what the player saw when the shot was made.
  -- But since the laser shot will be rendered with latency, too, the player will be
  -- able to integrate the latency via this visual feedback - provided that latency is stable over time.
  Action Laser dir ->
    lift (asks clientId) >>= notifyPlayers . GameEvent . LaserShot dir
  Action Ship dir ->
    adjustClient $ \c -> c { getShipAcceleration = sumCoords (coordsForDirection dir) $ getShipAcceleration c }
 where
  publish a = lift (asks clientId) >>= notifyEveryone . PlayerInfo a
  acceptCmd cmd = lift (asks clientId) >>= notifyEveryone . flip RunCommand cmd

mkServer :: Maybe ColorScheme -> Maybe ServerLogs -> Maybe ServerName -> ServerContent WorldParameters -> HamazedClientSideServer
mkServer color logs Nothing =
  Server (Local (fromMaybe NoLogs logs) (fromMaybe (ColorScheme $ rgb 3 2 2) color))
mkServer Nothing Nothing (Just (ServerName n)) =
  Server (Distant $ ServerName $ map toLower n)
mkServer _ (Just _) (Just _) =
  error "'--serverLogs' conflicts with '--serverName' (these options are mutually exclusive)."
mkServer (Just _) _ (Just _) =
  error "'--colorScheme' conflicts with '--serverName' (these options are mutually exclusive)."
