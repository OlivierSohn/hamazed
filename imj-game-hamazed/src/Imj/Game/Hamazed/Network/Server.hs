{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Network.Server
      ( Server
      , shutdown
      , mkServer
      , appSrv
      , gameScheduler
      , defaultPort
      , newServerState
      ) where

import           Imj.Prelude

import           Control.Concurrent(threadDelay, forkIO)
import           Control.Monad.IO.Unlift(MonadUnliftIO, MonadIO)
import           Control.Monad.Reader(runReaderT, lift, asks)
import           Control.Monad.State.Strict(StateT, MonadState, runStateT, execStateT, modify', get, gets, state)
import           Data.Char (isPunctuation, isSpace, toLower)
import           Data.Map.Strict(Map)
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
                                , readMVar, tryReadMVar, tryTakeMVar, putMVar, newEmptyMVar)

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Color.Types
import           Imj.Server.Internal.Types

import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.Text.ColorString(colored)
import           Imj.Graphics.Color
import           Imj.Music hiding(Do)
import           Imj.Server
import           Imj.Server.Log
import           Imj.Server.Connection

type HamazedServer = ServerState HamazedServerState

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

checkName :: PlayerName -> Either Text ()
checkName (PlayerName name) =
  if any ($ unpack name) [ null, any isPunctuation, any isSpace]
    then
      Left "Name cannot contain punctuation or whitespace, and cannot be empty"
    else
      Right ()

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


mkClientColorFromCenter :: ClientId -> Color8 Foreground -> Color8 Foreground
mkClientColorFromCenter (ClientId i) ref =
  let nColors = countHuesOfSameIntensity ref
      -- we want the following mapping:
      -- 0 -> 0
      -- 1 -> 1
      -- 2 -> -1
      -- 3 -> 2
      -- 4 -> -2
      -- ...
      dist = quot (succ i) 2
      n' = fromIntegral dist `mod` nColors
      n = if odd i then n' else -n'
  in rotateHue (fromIntegral n / fromIntegral nColors) ref

makePlayerName :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
               => SuggestedPlayerName -> m PlayerName
makePlayerName (SuggestedPlayerName sn) = do
  let go mayI = do
        let proposal = PlayerName $ pack $ maybe sn ((++) sn . show) mayI
        checkNameAvailability proposal >>= either
          (\_ -> go $ Just $ maybe (2::Int) succ mayI)
          (\_ -> return proposal)
  go Nothing

checkNameAvailability :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
                      => PlayerName -> m (Either Text ())
checkNameAvailability name =
  any ((== name) . getName . unClient) <$> gets clientsMap >>= \case
    True  -> return $ Left "Name is already taken"
    False -> return $ Right ()

{-# INLINABLE notifyPlayersN #-}
notifyPlayersN :: (MonadIO m, MonadState (ServerState HamazedServerState) m) => [ServerEventT HamazedServerState] -> m ()
notifyPlayersN evts =
  notifyN evts =<< gets onlyPlayersMap

{-# INLINABLE notifyPlayers #-}
notifyPlayers :: (MonadIO m, MonadState (ServerState HamazedServerState) m) => ServerEventT HamazedServerState -> m ()
notifyPlayers evt =
  notifyN [evt] =<< gets onlyPlayersMap


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


onLevelOutcome :: (MonadIO m, MonadState HamazedServer m)
               => LevelOutcome -> m ()
onLevelOutcome outcome =
  levelNumber <$> gets' levelSpecification >>= \levelN -> do
    adjustAll (\c -> case getState c of
      Just (Playing _) -> c { getState = Just ReadyToPlay }
      _ -> c)
    notifyEveryoneN $
      (GameInfo $ LevelResult levelN outcome):
      [GameInfo GameWon | outcome == Won && levelN == lastLevel]

    modify' $ mapState $ \s ->
     if outcome == Won && levelN < lastLevel
      then -- next level
        s { levelSpecification = (levelSpecification s) { levelNumber = succ levelN }
          , intent = IntentPlayGame Nothing
          }
      else -- end game
        s { levelSpecification = (levelSpecification s) { levelNumber = firstServerLevel }
          , intent = IntentSetup
          }
    gets' intent >>= \case
      IntentSetup ->
        -- make fresh clients become players
        adjustAll' (\c -> case getState c of
          Just (Playing _) -> error "inconsistent"
          Just ReadyToPlay -> Nothing
          Nothing -> Just $ c { getState = Just ReadyToPlay })
          >>= notifyEveryoneN . map (PlayerInfo Joins) . Set.toList
      IntentPlayGame _ -> return ()

-- | To avoid deadlocks, this function should be called only from inside a
-- transaction on ServerState.
{-# INLINABLE updateCurrentStatus #-}
updateCurrentStatus :: (MonadState HamazedServer m, MonadIO m)
                    => Maybe GameStatus
                    -- ^ the reference to take into account, if different from current status.
                    -> m (Maybe GameStatus)
updateCurrentStatus ref = gets' scheduledGame >>= tryReadMVar >>= maybe
  (return Nothing)
  (\(CurrentGame _ gamePlayers status _) -> do
    connectedPlayers' <- gets onlyPlayersMap
    let connectedPlayers = Map.keysSet connectedPlayers'
        missingPlayers = Set.difference gamePlayers connectedPlayers
        maybeNewStatus
          | null connectedPlayers = Just CancelledNoConnectedPlayer
          | null missingPlayers = case status of
              New -> Just $ Countdown 3 Running
              Paused _ statusBeforePause -> Just statusBeforePause
              Countdown n futureStatus -> Just $
                if n > 0
                  then
                    Countdown (pred n) futureStatus
                  else
                    futureStatus
              w@(WhenAllPressedAKey _ (Just n) _) ->
                Just $ w { countdown =
                  if n > 0
                    then Just $ pred n
                    else Nothing }
              WhenAllPressedAKey x Nothing havePressed ->
                if Map.null $ Map.filter (== False) havePressed
                  then
                    Just x
                  else
                    Nothing
              Running -> Nothing
              WaitingForOthersToEndLevel _ -> Nothing
              OutcomeValidated _ -> Nothing
              CancelledNoConnectedPlayer -> Nothing
          | otherwise = Just $ Paused missingPlayers $
              case status of
                Paused _ statusBeforePause -> statusBeforePause
                _ -> status
        newStatus = fromMaybe status maybeNewStatus
        oldStatus = fromMaybe status ref
    when (newStatus /= oldStatus) $
      onChangeStatus connectedPlayers' oldStatus newStatus
    return $ Just newStatus)

{-# INLINABLE onChangeStatus #-}
onChangeStatus :: (MonadState (ServerState HamazedServerState) m, MonadIO m)
               => Map ClientId (Client HamazedClient)
               -> GameStatus
               -> GameStatus
               -> m ()
onChangeStatus notified status newStatus = gets' scheduledGame >>= \game -> tryReadMVar game >>= maybe
  (return ())
  (\g -> do
    -- mayWorld can be concurrently modified from client handler threads, but only from
    -- inside a 'modifyMVar' on 'ServerState'. Since we are ourselves inside
    -- a 'modifyMVar' of 'ServerState', we guarantee that the 'takeMVar' inside
    -- 'swapMVar' won't block, because in the same 'modifyMVar' transaction, 'tryReadMVar'
    -- returned a 'Just'.
    liftIO $ void $ swapMVar game $! g {status' = newStatus}
    serverLog $ pure $ colored ("Game status change: " <> pack (show (status, newStatus))) yellow
    notifyN [EnterState $ PlayLevel newStatus] notified)

gameScheduler :: MVar HamazedServer -> IO ()
gameScheduler st =
  readMVar st >>= \(ServerState _ _ terminate (HamazedServerState _ _ _ _ _ _ mayGame)) ->
    if terminate
      then return ()
      else
        -- block until 'scheduledGame' contains a 'CurrentGame'
        readMVar mayGame >>= \(CurrentGame refWorld _ _ _) -> do
          let run x = modifyMVar st $ \s -> fmap swap $ runStateT (run' refWorld x) s
              toActOrNotToAct act maybeRefTime continuation = run act >>= \case
                Executed dt ->
                  continuation maybeRefTime dt
                NotExecutedTryAgainLater dt -> do
                  threadDelay $ fromIntegral $ toMicros dt
                  -- retry, but forget maybeRefTime because we waited,
                  -- which introduced a discontinuity.
                  toActOrNotToAct act Nothing continuation
                NotExecutedGameCanceled -> do
                  void $ liftIO $ tryTakeMVar mayGame -- we remove the game to stop the scheduler.

          toActOrNotToAct initializePlayers Nothing $ \_ dtInit -> do
            let go mayPrevUpdate mayDt = do
                  now <- getSystemTime
                  time <- maybe
                    (return now)
                    (\dt -> do
                      let baseTime = fromMaybe now mayPrevUpdate
                          update = addDuration dt baseTime
                      threadDelay $ fromIntegral $ toMicros $ now...update
                      return update)
                    mayDt
                  toActOrNotToAct (stepWorld time) (Just time) go
            go Nothing dtInit
          gameScheduler st
 where
  -- run the action if the world matches the current world and the game was
  -- not terminated
  --run' :: WorldId
  --     -> StateT (ServerState s) IO (Maybe (Time Duration System))
  --     -> StateT (ServerState s) IO RunResult
  run' refWid act =
    go >>= \res -> do
      case res of
        NotExecutedTryAgainLater _ -> stopMusic
        NotExecutedGameCanceled -> stopMusic
        Executed _ -> return ()
      return res

   where
    stopMusic = gets unServerState >>= \(HamazedServerState _ _ _ _ _ _ game) ->
      tryTakeMVar game >>= maybe
        (return ())
        (\g@(CurrentGame _ _ _ s) -> do
          let (newScore, notesChanges) = stopScore s
          case notesChanges of
            [] -> return ()
            _:_ -> notifyPlayersN $ map (flip PlayMusic SineSynth) notesChanges
          putMVar game $ g{score = newScore})

    go = get >>= \(ServerState _ _ terminate (HamazedServerState _ _ _ _ _ _ game)) ->
      if terminate
        then do
          serverLog $ pure $ colored "Terminating game" yellow
          return NotExecutedGameCanceled
        else
          -- we use 'tryReadMVar' to /not/ block here, as we are inside a modifyMVar.
          liftIO (tryReadMVar game) >>= maybe
            (do serverError "logic : mayGame is Nothing"
                return NotExecutedGameCanceled)
            (\(CurrentGame curWid _ _ _) ->
              if refWid /= curWid
                then do
                  serverLog $ pure $ colored ("The world has changed: " <> pack (show (curWid, refWid))) yellow
                  return NotExecutedGameCanceled -- the world has changed
                else
                  updateCurrentStatus Nothing >>= maybe (return NotExecutedGameCanceled) (\case
                    Paused _ _ ->
                      return $ NotExecutedTryAgainLater $ fromSecs 1
                    CancelledNoConnectedPlayer -> do
                      onLevelOutcome $ Lost "All players left"
                      return NotExecutedGameCanceled
                    Running ->
                      Executed <$> act
                    WaitingForOthersToEndLevel _ ->
                      return $ NotExecutedTryAgainLater $ fromSecs 0.1
                    WhenAllPressedAKey _ (Just _) _ ->
                      return $ NotExecutedTryAgainLater $ fromSecs 1 -- so that the Just corresponds to seconds
                    WhenAllPressedAKey _ Nothing _ ->
                      return $ NotExecutedTryAgainLater $ fromSecs 0.1
                    Countdown _ _ ->
                      return $ NotExecutedTryAgainLater $ fromSecs 1
                    OutcomeValidated outcome -> do
                      onLevelOutcome outcome
                      requestWorld
                      return NotExecutedGameCanceled
                    -- these values are never supposed to be used here:
                    New -> do
                      serverError "logic : newStatus == New"
                      return NotExecutedGameCanceled))

--  initializePlayers :: StateT (ServerState s) IO (Maybe (Time Duration System))
  initializePlayers = liftIO getSystemTime >>= \start -> do
    -- we add one second to take into account the fact that start of game is delayed by one second.
    adjustAll $ \c -> c { getShipSafeUntil = Just $ addDuration (fromSecs 6) start }
    return Nothing

--  stepWorld :: Time Point System -> StateT (ServerState s) IO (Maybe (Time Duration System))
  stepWorld now = do
    let !zero = zeroCoords
        mult = initalGameMultiplicator
    accs <- Map.mapMaybe
      (\p ->
          let !acc = getShipAcceleration $ unClient p
            in if acc == zero
              then Nothing
              else Just acc)
      <$> gets onlyPlayersMap
    updateSafeShips >>= \shipsLostArmor ->
      updateVoice >>= \noteChange ->
        notifyPlayersN (map (flip PlayMusic SineSynth) noteChange ++ [GameEvent $ PeriodicMotion accs shipsLostArmor])
    adjustAll $ \p -> p { getShipAcceleration = zeroCoords }
    return $ Just $ toSystemDuration mult gameMotionPeriod
   where
    updateVoice =
      gets unServerState >>= \s ->
        liftIO $ modifyMVar
          (scheduledGame s)
          (\g -> let (newScore, noteChange) = stepScore $ score g in return (g {score = newScore}, noteChange))

    updateSafeShips = adjustAll' $ \c@(HamazedClient _ _ mayTimeUnsafe _ _ _) ->
      maybe
        Nothing
        (\timeUnsafe ->
          if timeUnsafe < now
            then
              Just $ c { getShipSafeUntil = Nothing }
            else
              Nothing)
        mayTimeUnsafe

data RunResult =
    NotExecutedGameCanceled
  | NotExecutedTryAgainLater !(Time Duration System)
  -- ^ withe the duration to sleep before retrying
  | Executed !(Maybe (Time Duration System))
  -- ^ With an optional duration to wait before the next iteration


addStats :: Map Properties Statistics
         -> WorldId
         -- ^ if this 'WorldId' doesn't match with the 'WorldId' of 'worldCreation',
         -- nothing is done because it is obsolete (eventhough the server cancels obsolete requests,
         -- this case could occur if the request was cancelled just after the non-result was
         -- sent by the client).
         -> ClientHandlerIO HamazedServerState ()
addStats stats key =
  gets' worldCreation >>= \wc@(WorldCreation st wid _ prevStats) -> do
    let newStats = safeMerge mergeStats prevStats stats
    bool
      (serverLog $ pure $ colored ("Obsolete key " <> pack (show key)) blue)
      (case st of
          -- drop newStats if world is already created.
        Created -> return ()
        CreationAssigned _ -> do
          log $ colored (pack $ show newStats) white
          modify' $ mapState $ \s -> s { worldCreation = wc { creationStatistics = newStats } })
      $ wid == key


participateToWorldCreation :: WorldId
                           -- ^ if this 'WorldId' doesn't match with the 'WorldId' of 'worldCreation',
                           -- nothing is done because it is obsolete (eventhough the server cancels obsolete requests,
                           -- this case could occur if the request was cancelled just after the non-result was
                           -- sent by the client).
                           -> ClientHandlerIO HamazedServerState ()
participateToWorldCreation key = asks clientId >>= \origin ->
  gets' worldCreation >>= \wc@(WorldCreation st wid _ _) ->
    bool
      (serverLog $ pure $ colored ("Obsolete key " <> pack (show key)) blue)
      (case st of
        Created ->
          serverLog $ pure $ colored ("World " <> pack (show key) <> " is already created.") blue
        CreationAssigned assignees -> do
          let prevSize = Set.size assignees
              single = Set.singleton origin
              newAssignees = Set.union single assignees
              newSize = Set.size newAssignees
          unless (newSize == prevSize) $
            -- a previously assigned client has disconnected, reconnects and sends a non-result
            serverLog $ pure $ colored ("Adding assignee : " <> pack (show origin)) blue
          modify' $ mapState $ \s -> s { worldCreation = wc { creationState = CreationAssigned newAssignees
                                                 } }
          gets clientsMap >>= requestWorldBy . flip Map.restrictKeys single)
      $ wid == key

onDelta :: (MonadIO m, MonadState HamazedServer m)
        => Int
        -> SharedEnumerableValueKey
        -> m ()
onDelta i key = onChangeWorldParams $ \wp -> case key of
  BlockSize -> case wallDistrib wp of
    p@(WallDistribution prevSize _) ->
      let adjustedSize
           | newSize < minBlockSize = minBlockSize
           | newSize > maxBlockSize = maxBlockSize
           | otherwise = newSize -- TODO define an upper bound and use it for the slider.
           where newSize = prevSize + i
      in bool
        (Just $ wp { wallDistrib = p { blockSize' = adjustedSize } })
        Nothing $
        adjustedSize == prevSize
  WallProbability -> case wallDistrib wp of
    p@(WallDistribution _ prevProba) ->
      let adjustedProba
           | newProba < minWallProba = minWallProba
           | newProba > maxWallProba = maxWallProba
           | otherwise = newProba
           where
             newProba = wallProbaIncrements * fromIntegral (round (newProba' / wallProbaIncrements) :: Int)
             newProba' = minWallProba + wallProbaIncrements * fromIntegral nIncrements
             nIncrements = i + round ((prevProba - minWallProba) / wallProbaIncrements)
      in bool
        (Just $ wp { wallDistrib = p { wallProbability' = adjustedProba } })
        Nothing $
        adjustedProba == prevProba

onChangeWorldParams :: (MonadIO m, MonadState HamazedServer m)
                    => (WorldParameters -> Maybe WorldParameters)
                    -> m ()
onChangeWorldParams f =
  state (\s ->
    let mayNewParams = f prevParams
        prevParams = worldParameters $ unServerState s
    in (mayNewParams
      , maybe id (\newParams -> mapState (\s' -> s' { worldParameters = newParams })) mayNewParams s))
    >>= maybe (return ()) onChange
 where
  onChange p = do
    notifyEveryone $ OnWorldParameters p
    requestWorld

changeWorldShape :: WorldShape -> WorldParameters -> Maybe WorldParameters
changeWorldShape d p =
  bool (Just $ p { worldShape = d }) Nothing $ d == worldShape p


newServerState :: ServerLogs -> ColorScheme -> IO (ServerState HamazedServerState)
newServerState logs colorScheme = do
  c <- mkCenterColor colorScheme
  let lvSpec = LevelSpec firstServerLevel CannotOvershoot
      params = initialParameters
  mkServerState logs . HamazedServerState mkGameTiming lvSpec params
            (mkWorldCreation $ WorldSpec lvSpec Set.empty params)
            IntentSetup c <$> newEmptyMVar


mkWorldCreation :: WorldSpec -> WorldCreation
mkWorldCreation spec = WorldCreation (CreationAssigned Set.empty) (WorldId 0) spec Map.empty

mkGameTiming :: GameTiming
mkGameTiming = GameTiming Nothing initalGameMultiplicator

mkCenterColor :: ColorScheme -> IO (Color8 Foreground)
mkCenterColor (ColorScheme c) = return c
mkCenterColor UseServerStartTime = do
  t <- getCurrentSecond
  let !ref = rgb 3 2 0
      nColors = countHuesOfSameIntensity ref
      n = t `mod` nColors
  return $ rotateHue (fromIntegral n / fromIntegral nColors) ref

mkServer :: Maybe ColorScheme -> Maybe ServerLogs -> Maybe ServerName -> ServerContent WorldParameters -> HamazedClientSideServer
mkServer color logs Nothing =
  Server (Local (fromMaybe NoLogs logs) (fromMaybe (ColorScheme $ rgb 3 2 2) color))
mkServer Nothing Nothing (Just (ServerName n)) =
  Server (Distant $ ServerName $ map toLower n)
mkServer _ (Just _) (Just _) =
  error "'--serverLogs' conflicts with '--serverName' (these options are mutually exclusive)."
mkServer (Just _) _ (Just _) =
  error "'--colorScheme' conflicts with '--serverName' (these options are mutually exclusive)."

-- Game Timing:
{-
{-# INLINABLE onMove #-}
onMove :: (MonadState AppState m, MonadIO m)
       => Map ClientId (Coords Vel)
       -> [ClientId]
       -> m ()
onMove accelerations shipsLosingArmor = getGameState >>= \(GameState _ m@(Multiplicator mv) world a b c d e) ->
  liftIO getSystemTime >>= \t -> do
    let nextTime = addDuration (toSystemDuration m gameMotionPeriod) t
    putGameState $ GameState (Just nextTime) (Multiplicator (mv + 0.01)) (moveWorld accelerations shipsLosingArmor world) a b c d e
    onHasMoved t
-}
