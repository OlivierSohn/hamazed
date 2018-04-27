{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Network.Server
      ( ClientNode(..)
      , Server
      , shutdown
      , mkServer
      , appSrv
      , gameScheduler
      , defaultPort
      ) where

import           Imj.Prelude

import           Control.Concurrent(threadDelay, forkIO)
import           Control.Monad.IO.Unlift(MonadUnliftIO, MonadIO)
import           Control.Monad.Reader(runReaderT, lift, asks)
import           Control.Monad.State.Strict(StateT, MonadState, runStateT, execStateT, modify', get, gets, state)
import           Data.Char (isPunctuation, isSpace, toLower)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map(map, mapMaybe, union, filter, take, keys, adjust, keysSet
                                      , insert, null, empty, mapAccumWithKey, updateLookupWithKey
                                      , restrictKeys, traverseWithKey, lookup, toList, partition)
import qualified Data.List as List(intercalate)
import           Data.Set (Set)
import qualified Data.Set as Set (difference, union, lookupMin, empty, singleton, null, insert, delete, size)
import           Data.Maybe(isJust)
import           Data.Text(pack, unpack, justifyRight)
import qualified Data.Text as Text(intercalate)
import           Data.Tuple(swap)
import           Network.WebSockets
                  (PendingConnection, WebSocketsData(..), Connection, DataMessage(..),
                  -- the functions on the mext line throw(IO), hence we catch exceptions
                  -- to remove the client from the map when needed.
                   acceptRequest, sendBinaryData, sendClose, sendPing, receiveData)
import           Network.WebSockets.Connection(sendDataMessages)
import           UnliftIO.Exception (SomeException(..), try)
import           UnliftIO.MVar (MVar
                                , modifyMVar_, modifyMVar, swapMVar
                                , readMVar, tryReadMVar, tryTakeMVar, putMVar)

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.Network.Class.ClientNode
import           Imj.Graphics.Color.Types

import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.Text.ColorString(ColorString, intercalate, colored)
import           Imj.Graphics.Color
import           Imj.Music hiding(Do)
import           Imj.Log

type ClientHandlerIO = StateT ServerState (ReaderT ConstClient IO)

-- | Data of a 'Client' that will never change.
data ConstClient = ConstClient {
    connection :: {-# UNPACK #-} !Connection
  , shipId :: {-# UNPACK #-} !ShipId
  , lifecycle :: !ClientLifecycle
}

log :: ColorString -> ClientHandlerIO ()
log msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs -> do
    i <- asks shipId
    idStr <- showId i
    liftIO $ baseLog $ intercalate "|"
      [ idStr
      , msg
      ]

warning :: Text -> ClientHandlerIO ()
warning msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs -> do
    i <- asks shipId
    idStr <- showId i
    liftIO $ baseLog $ intercalate "|"
      [ idStr
      , colored msg orange
      ]

showId :: (MonadIO m, MonadState ServerState m) => ShipId -> m ColorString
showId i = do
  color <- fromMaybe white <$> gets (clientColor i)
  return $ colored (pack $ show i) color

showClient :: Client -> ColorString
showClient c = colored (pack $ show c) $ gray 16

serverLog :: (MonadIO m, MonadState ServerState m)
          => m ColorString
          -> m ()
serverLog msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs ->
    msg >>= baseLog

defaultPort :: ServerPort
defaultPort = ServerPort 10052

mkServer :: Maybe ColorScheme -> Maybe ServerLogs -> Maybe ServerName -> ServerContent -> Server
mkServer color logs Nothing =
  Server (Local (fromMaybe NoLogs logs) (fromMaybe (ColorScheme $ rgb 3 2 2) color))
mkServer Nothing Nothing (Just (ServerName n)) =
  Server (Distant $ ServerName $ map toLower n)
mkServer _ (Just _) (Just _) =
  error "'--serverLogs' conflicts with '--serverName' (these options are mutually exclusive)."
mkServer (Just _) _ (Just _) =
  error "'--colorScheme' conflicts with '--serverName' (these options are mutually exclusive)."

{-# INLINABLE notifyEveryone #-}
notifyEveryone :: (MonadIO m, MonadState ServerState m) => ServerEvent -> m ()
notifyEveryone evt =
  notifyN [evt] =<< gets clientsMap

{-# INLINABLE notifyEveryoneN #-}
notifyEveryoneN :: (MonadIO m, MonadState ServerState m) => [ServerEvent] -> m ()
notifyEveryoneN evts =
  notifyN evts =<< gets clientsMap

{-# INLINABLE notifyPlayersN #-}
notifyPlayersN :: (MonadIO m, MonadState ServerState m) => [ServerEvent] -> m ()
notifyPlayersN evts =
  notifyN evts =<< gets onlyPlayersMap

{-# INLINABLE notifyPlayers #-}
notifyPlayers :: (MonadIO m, MonadState ServerState m) => ServerEvent -> m ()
notifyPlayers evt =
  notifyN [evt] =<< gets onlyPlayersMap

{-# INLINABLE notifyN #-}
-- | Uses sendDataMessage which is at a lower-level than sendBinaryData
-- to factorize serialization.
--
-- In case some connections are closed, the corresponding clients are silently removed
-- from the Map, and we continue the processing.
notifyN :: (MonadIO m, MonadState ServerState m)
        => [ServerEvent]
        -> Map ShipId Client
        -> m ()
notifyN evts =
  void . Map.traverseWithKey
    (\i client -> sendAndHandleExceptions evts (getConnection client) i)

{-# INLINABLE notify #-}
notify :: (MonadIO m, MonadState ServerState m) => Connection -> ShipId -> ServerEvent -> m ()
notify conn sid evt =
  sendAndHandleExceptions [evt] conn sid

notifyClient :: ServerEvent -> ClientHandlerIO ()
notifyClient evt = do
  conn <- asks connection
  sid <- asks shipId
  notify conn sid evt

{-# INLINABLE sendAndHandleExceptions #-}
sendAndHandleExceptions :: (MonadIO m, MonadState ServerState m)
                        => [ServerEvent] -> Connection -> ShipId -> m ()
sendAndHandleExceptions [] _ _ = return () -- sendDataMessages throws on empty list
sendAndHandleExceptions evts conn i =
  liftIO (try $ sendDataMessages conn msgs) >>= either
    (\(e :: SomeException) -> onBrokenClient "" (Just ("sending", "ServerEvent", evts)) e i)
    return
 where
  !msgs = map (Binary . toLazyByteString) evts

appSrv :: MVar ServerState -> PendingConnection -> IO ()
appSrv st pending =
  acceptRequest pending >>= appSrv' st

-- We handle all "normal" exceptions (which happen due to network failures or players disconnections)
-- inside so that a broken connection of another client while broadcasting a message doesn't impact this client.
appSrv' :: MVar ServerState -> Connection -> IO ()
appSrv' st conn =
  -- Currently, we cannot have a 'Client' in a disconnected state, this is why
  -- we do a special case for Connect. But ideally we should be able to disconnect a client.
  -- and reconnect it in the loop. To do that, when we disconnect a client, it will not
  -- be removed from the map, instead its state will be Disconnected. (TODO)
  receiveData conn >>= \case
    Connect sn@(SuggestedPlayerName suggestedName) cliType -> either
      (sendBinaryData conn . ConnectionRefused . InvalidName sn)
      (\_ -> modifyMVar st (fmap swap . runStateT takeShipId) >>=
               runReaderT (handleClient st sn cliType) . uncurry (ConstClient conn))
      $ checkName $ PlayerName $ pack suggestedName
    msg -> error $ "First sent message should be 'Connect'. " ++ show msg

handleClient :: MVar ServerState -> SuggestedPlayerName -> ServerOwnership -> ReaderT ConstClient IO ()
handleClient st sn cliType = do
  i <- asks shipId
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
      asks lifecycle >>= \case
        NewClient ->
          log "Is a new client"
        ReconnectingClient gameConnectedPlayers -> do
          log "Is a reconnecting client"
          gets worldCreation >>= \(WorldCreation creationSt wid _ _) -> case creationSt of
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
  | ReconnectingClient !(Set ShipId)
  deriving(Show)

takeShipId :: StateT ServerState IO (ShipId, ClientLifecycle)
takeShipId =
  takeDisconnectedShipId >>= maybe
    (state $ \s ->
      let newShipId = getNextShipId $ getClients s
      in ( (newShipId, NewClient)
         , s { getClients = (getClients s) { getNextShipId = succ newShipId } } ))
    (\(gameConnectedPlayers, disconnected) -> do
      when (Set.null gameConnectedPlayers) $
        serverError "the current game has no connected player" -- TODO support that
      return (disconnected, ReconnectingClient gameConnectedPlayers))
 where
  takeDisconnectedShipId :: StateT ServerState IO (Maybe (Set ShipId, ShipId)) -- (connected, disconnected)
  takeDisconnectedShipId =
    get >>= \(ServerState _ _ _ _ _ _ _ _ terminate game) ->
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

addClient :: SuggestedPlayerName -> ServerOwnership -> ClientHandlerIO ()
addClient sn cliType = do
  conn <- asks connection
  i <- asks shipId
  playerColor <- mkClientColor i
  presentClients <- do
    name <- makePlayerName sn
    let client@(Client _ _ _ _ _ _ _ c) = mkClient name playerColor conn cliType
    -- this call is /before/ addClient to avoid sending redundant info to client.
    notifyEveryoneN $ map (RunCommand i) [AssignName name, AssignColor c]
    -- order matters, see comment above.
    modify' $ \ s ->
      let clients = getClients s
      in s { getClients =
              clients { getClients' =
                Map.insert i client $ getClients' clients } }
    serverLog $ (\strId -> colored "Add client" green <> "|" <> strId <> "|" <> showClient client) <$> showId i
    Map.map
      (\(Client n _ _ _ _ _ _ color) -> PlayerEssence n Present color)
      <$> gets clientsMap
  gets worldParameters >>= notifyClient . ConnectionAccepted i presentClients


mkClientColor :: (MonadIO m, MonadState ServerState m)
              => ShipId -> m (Color8 Foreground)
mkClientColor (ShipId i) = do
  ref <- gets centerColor
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
  return $ rotateHue (fromIntegral n / fromIntegral nColors) ref

makePlayerName :: (MonadIO m, MonadState ServerState m)
               => SuggestedPlayerName -> m PlayerName
makePlayerName (SuggestedPlayerName sn) = do
  let go mayI = do
        let proposal = PlayerName $ pack $ maybe sn ((++) sn . show) mayI
        checkNameAvailability proposal >>= either
          (\_ -> go $ Just $ maybe (2::Int) succ mayI)
          (\_ -> return proposal)
  go Nothing

checkNameAvailability :: (MonadIO m, MonadState ServerState m)
                      => PlayerName -> m (Either Text ())
checkNameAvailability name =
  any ((== name) . getName) <$> gets clientsMap >>= \case
    True  -> return $ Left "Name is already taken"
    False -> return $ Right ()

shutdown :: Text -> StateT ServerState IO ()
shutdown reason = do
  serverLog $ pure $ colored "Server shutdown" red <> "|" <> colored reason (rgb 4 1 1)
  modify' $ \s -> s { shouldTerminate = True }
  Map.keysSet <$> gets clientsMap >>= mapM_ (disconnect $ ServerShutdown reason)

-- It's important that this function doesn't throw any exception.
onBrokenClient :: (Show a, MonadIO m, MonadState ServerState m)
               => Text
               -- ^ Describes the type of thread in which the exception was raised
               -> Maybe (Text, Text, [a])
               -- ^ A list of values that will be added to the log.
               -> SomeException
               -> ShipId
               -> m ()
onBrokenClient threadCategory infos e i = do
  log'
  disconnect (BrokenClient $ pack $ show e) i
 where
  log' = serverLog $
    showId i >>= \strId -> pure $ firstLine strId <> logDetailedException infos e
  firstLine s = intercalate "|"
    [ colored "Exception" (rgb 4 2 0)
    , colored (justifyRight 10 '.' threadCategory) yellow
    , s
    ]

disconnect :: (MonadIO m, MonadState ServerState m) => DisconnectReason -> ShipId -> m ()
disconnect r i =
  tryRemoveClient >>= maybe
    (do serverLog $ (<> " was already removed.") <$> showId i
        return ())
    (\c@(Client (PlayerName name) _ ownership _ _ _ _ _) -> do
        -- If the client owns the server, we shutdown connections to /other/ clients first.
        when (ownership == ClientOwnsServer) $ do
          gets clientsMap >>= void . Map.traverseWithKey
            (\j client' -> do
                let msg = "[" <> name <> "] disconnection (hosts the Server) < " <> pack (show r)
                -- Because we pass a 'ServerShutdown', 'closeConnection' won't
                -- use the clients map so it's safe to just clear the client map
                -- /after/ the entire traversal was done, we won't have infinite recursion.
                closeConnection (ServerShutdown msg) j client')
          removeAllClients
        -- Finally, shutdown the client connection.
        closeConnection r i c)
 where
  closeConnection :: (MonadIO m, MonadState ServerState m) => DisconnectReason -> ShipId -> Client -> m ()
  closeConnection reason cid c@(Client (PlayerName playerName) conn _ _ _ _ _ color) = do
    serverLog $ pure $
      colored "Close connection" yellow <>
      "|" <>
      colored (pack $ show cid) color <>
      "|" <>
      showClient c
    -- If possible, notify the client about the disconnection
    case reason of
      BrokenClient _ ->
        -- we can't use the client connection anymore.
        -- on its side, the client will probably receive an exception when reading or sending data.
        return ()
      _ -> do
        notify conn cid $ Disconnected reason
        liftIO $ sendClose conn $
          "[" <> playerName <> "] disconnection < " <> pack (show reason)
    -- if needed, notify other clients about the disconnection of client
    case reason of
      BrokenClient e ->
        notifyEveryone $ RunCommand cid $ Leaves $ ConnectionError e
      ClientShutdown ->
        notifyEveryone $ RunCommand cid $ Leaves Intentional
      ServerShutdown _ ->
        return () -- no need to notify other clients, as they will be diconnected too,
                  -- hence they will receive a server shutdown notification.
    -- if needed, request a new world
    case reason of
      ServerShutdown _ ->
        return ()
      _ -> do
        unAssign cid
        gets intent >>= \case
          IntentSetup -> requestWorld
          IntentPlayGame _ -> return ()

  tryRemoveClient = state $ \s ->
    let clients = getClients s
        (mayClient, newClients) =
          Map.updateLookupWithKey
            (\_ _ -> Nothing)
            i
            $ getClients' clients
    in (mayClient, s { getClients = clients { getClients' = newClients } })
  removeAllClients = modify' $ \s ->
    s { getClients = (getClients s) { getClients' = Map.empty } }

{-# INLINE clientsMap #-}
clientsMap :: ServerState -> Map ShipId Client
clientsMap = getClients' . getClients

onlyPlayersMap :: ServerState -> Map ShipId Client
onlyPlayersMap = Map.filter (isJust . getState) . clientsMap

onlyPlayersIds :: ServerState -> Set ShipId
onlyPlayersIds =
  Map.keysSet . onlyPlayersMap

findClient :: ShipId -> ServerState -> Maybe Client
findClient i = Map.lookup i . clientsMap

clientColor :: ShipId -> ServerState -> Maybe (Color8 Foreground)
clientColor i = fmap getColor <$> findClient i

gameError :: String -> ClientHandlerIO ()
gameError = error' "Game"

handlerError :: String -> ClientHandlerIO ()
handlerError = error' "Handler"

error' :: String -> String -> ClientHandlerIO ()
error' from msg = do
  log $ colored (pack txt) red
  notifyClient $ ServerError txt
  error txt
 where
  txt = List.intercalate "|" [from, "error from Server", msg]

serverError :: (MonadIO m, MonadState ServerState m)
            => String
            -> m ()
serverError msg = do
  serverLog $ pure $ colored (pack txt) red
  notifyEveryone $ ServerError txt
  error txt
 where
  txt = List.intercalate "|" ["Server error from Server", msg]

handleIncomingEvent :: ClientEvent -> ClientHandlerIO ()
handleIncomingEvent =
  withArgLogged handleIncomingEvent'

{-# INLINABLE withArgLogged #-}
withArgLogged :: (Show a) => (a -> ClientHandlerIO b) -> a -> ClientHandlerIO b
withArgLogged act arg = do
  log $ colored " >> " (gray 18) <> keepExtremities (show arg)
  res <- act arg
  log $ colored " <<" (gray 18)
  return res

handleIncomingEvent' :: ClientEvent -> ClientHandlerIO ()
handleIncomingEvent' = \case
  Connect (SuggestedPlayerName sn) _ ->
    handlerError $ "already connected : " <> sn

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
    asks shipId >>= disconnect ClientShutdown -- will do the corresponding 'notifyEveryone $ RunCommand'
  Do cmd -> do
   case cmd of
    Put (ColorSchemeCenter color) -> do
      modify' $ \s -> s { centerColor = color }
      gets clientsMap >>= Map.traverseWithKey (\i c -> do
        col <- mkClientColor i
        return c { getColor = col })
          >>= \newClients -> do
            setClients newClients
            notifyEveryoneN $
              map (\(k, c) -> RunCommand k (AssignColor $ getColor c)) $
              Map.toList newClients
    Put (WorldShape s) ->
      onChangeWorldParams $ changeWorldShape s
    Succ x -> onDelta 1    x
    Pred x -> onDelta (-1) x
   publish $ Done cmd

  Report (Get ColorSchemeCenterKey) ->
    gets centerColor >>= notifyClient . Reporting . Put . ColorSchemeCenter
  Report (Get WorldShapeKey) ->
    worldShape <$> gets worldParameters >>= notifyClient . Reporting . Put . WorldShape

  ExitedState Excluded -> gets intent >>= \case
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
  ExitedState Setup -> gets intent >>= \case
    IntentSetup -> do
      modify' $ \s -> s { intent = IntentPlayGame Nothing }
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
    gets intent >>= \case
      IntentPlayGame Nothing ->
        modify' $ \s -> s { intent = IntentPlayGame $ Just outcome }
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
          (playersPlaying, playersDonePlaying) = Map.partition (playing . getState) players'
          gameStatus =
            if null playersPlaying
              then
                WhenAllPressedAKey (OutcomeValidated outcome) (Just 2) $ Map.map (const False) playersDonePlaying
              else
                WaitingForOthersToEndLevel $ Map.keysSet playersPlaying
      notifyN [EnterState $ PlayLevel gameStatus] playersDonePlaying
      gets scheduledGame >>= \g -> liftIO (tryTakeMVar g) >>= maybe
        (warning "LevelEnded sent while game is Nothing")
        (\game -> void $ liftIO $ putMVar g $! game { status' = gameStatus })
  CanContinue next ->
    gets scheduledGame >>= \g -> liftIO (tryReadMVar g) >>= maybe
      (warning "CanContinue sent while game is Nothing")
      (\game -> do
          let curStatus = status' game
          case curStatus of
            WhenAllPressedAKey x Nothing havePressed -> do
              when (x /= next) $ error $ "inconsistent:"  ++ show (x,next)
              i <- lift $ asks shipId
              let newHavePressed = Map.insert i True havePressed
                  intermediateStatus = WhenAllPressedAKey x Nothing newHavePressed
              liftIO $ void $ swapMVar g $! game { status' = intermediateStatus }
              -- update to avoid state where the map is equal to all players:
              void $ updateCurrentStatus $ Just curStatus
            _ -> error $ "inconsistent:"  ++ show curStatus)

  WorldProposal wid mkEssenceRes stats -> case mkEssenceRes of
    Impossible errs -> gets intent >>= \case
      IntentSetup -> gets levelSpecification >>= notifyPlayers . GameInfo . CannotCreateLevel errs . levelNumber
      IntentPlayGame _ ->
        fmap levelNumber (gets levelSpecification) >>= \ln -> do
          notifyPlayers $ GameInfo $ CannotCreateLevel errs ln
          -- TODO before launching the game we should check that it is possible to create all levels.
          serverError $ "Could not create level " ++ show ln ++ ":" ++ unpack (Text.intercalate "\n" errs)

    NeedMoreTime -> addStats stats wid
    Success essence -> get >>= \(ServerState _ _ _ levelSpec _ (WorldCreation st key spec prevStats) _ _ _ _) -> bool
      (serverLog $ pure $ colored ("Dropped obsolete world " <> pack (show wid)) blue)
      (case st of
        Created ->
          serverLog $ pure $ colored ("Dropped already created world " <> pack (show wid)) blue
        CreationAssigned _ -> do
          -- This is the first valid world essence, so we can cancel the request
          cancelWorldRequest
          let !newStats = safeMerge mergeStats prevStats stats
          log $ colored (pack $ show newStats) white
          modify' $ \s -> s { worldCreation = WorldCreation Created key spec newStats }
          notifyPlayers $ ChangeLevel (mkLevelEssence levelSpec) essence key)
      $ key == wid
  CurrentGameState wid mayGameStateEssence -> maybe
    (handlerError $ "Could not get GameState " ++ show wid)
    (\gameStateEssence ->
      gets scheduledGame >>= liftIO . tryReadMVar >>= \case
        Just (CurrentGame _ gamePlayers (Paused _ _) _) -> do
          disconnectedPlayerKeys <- Set.difference gamePlayers <$> gets onlyPlayersIds
          flip Map.restrictKeys disconnectedPlayerKeys <$> gets clientsMap >>=
            notifyN [PutGameState gameStateEssence wid]
        invalid -> handlerError $ "CurrentGameState sent while game is " ++ show invalid)
    mayGameStateEssence

  IsReady wid -> do
    adjustClient $ \c -> c { getCurrentWorld = Just wid }
    gets intent >>= \case
      IntentSetup ->
        -- (follow-up from 'ExitedState Excluded')
        -- Allow the client to setup the world, now that the world contains its ship.
        notifyClient $ EnterState Setup
      IntentPlayGame maybeOutcome ->
        get >>= \(ServerState _ (Clients clients _) _ _ _ (WorldCreation _ lastWId _ _) _ _ _ game) ->
         liftIO (tryReadMVar game) >>= maybe
          (do
            adjustClient $ \c -> c { getState = Just ReadyToPlay }
            -- start the game when all players have the right world
            playersKeys <- gets onlyPlayersIds
            let playersAllReady =
                  all ((== Just lastWId) . getCurrentWorld) $ Map.restrictKeys clients playersKeys
            when playersAllReady $ do
              adjustAll $ \c ->
                case getState c of
                  Just ReadyToPlay ->
                    c { getState = Just $ Playing Nothing
                      , getShipAcceleration = zeroCoords }
                  _ -> c
              -- 'putMVar' is non blocking because all game changes are done inside a modifyMVar
              -- of 'ServerState' and we are inside one.
              void $ liftIO $ putMVar game $ mkCurrentGame lastWId playersKeys)
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
    lift (asks shipId) >>= notifyPlayers . GameEvent . LaserShot dir
  Action Ship dir ->
    adjustClient $ \c -> c { getShipAcceleration = sumCoords (coordsForDirection dir) $ getShipAcceleration c }
 where
  publish a = lift (asks shipId) >>= notifyEveryone . PlayerInfo a
  acceptCmd cmd = lift (asks shipId) >>= notifyEveryone . flip RunCommand cmd


onLevelOutcome :: (MonadIO m, MonadState ServerState m)
               => LevelOutcome -> m ()
onLevelOutcome outcome =
  levelNumber <$> gets levelSpecification >>= \levelN -> do
    adjustAll (\c -> case getState c of
      Just (Playing _) -> c { getState = Just ReadyToPlay }
      _ -> c)
    notifyEveryoneN $
      (GameInfo $ LevelResult levelN outcome):
      [GameInfo GameWon | outcome == Won && levelN == lastLevel]

    modify' $ \s ->
     if outcome == Won && levelN < lastLevel
      then -- next level
        s { levelSpecification = (levelSpecification s) { levelNumber = succ levelN }
          , intent = IntentPlayGame Nothing
          }
      else -- end game
        s { levelSpecification = (levelSpecification s) { levelNumber = firstServerLevel }
          , intent = IntentSetup
          }
    gets intent >>= \case
      IntentSetup ->
        -- make fresh clients become players
        adjustAll' (\c -> case getState c of
          Just (Playing _) -> error "inconsistent"
          Just ReadyToPlay -> Nothing
          Nothing -> Just $ c { getState = Just ReadyToPlay })
          >>= notifyEveryoneN . map (PlayerInfo Joins)
      IntentPlayGame _ -> return ()

-- | To avoid deadlocks, this function should be called only from inside a
-- transaction on ServerState.
{-# INLINABLE updateCurrentStatus #-}
updateCurrentStatus :: (MonadState ServerState m, MonadIO m)
                    => Maybe GameStatus
                    -- ^ the reference to take into account, if different from current status.
                    -> m (Maybe GameStatus)
updateCurrentStatus ref = gets scheduledGame >>= tryReadMVar >>= maybe
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
onChangeStatus :: (MonadState ServerState m, MonadIO m)
               => Map ShipId Client
               -> GameStatus
               -> GameStatus
               -> m ()
onChangeStatus notified status newStatus = gets scheduledGame >>= \game -> tryReadMVar game >>= maybe
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

gameScheduler :: MVar ServerState -> IO ()
gameScheduler st =
  readMVar st >>= \(ServerState _ _ _ _ _ _ _ _ terminate mayGame) ->
    if terminate
      then return ()
      else
        -- block until 'scheduledGame' contains a 'CurrentGame'
        readMVar mayGame >>= \(CurrentGame refWorld _ _ _) -> do
          let run x = modifyMVar st $ fmap swap . runStateT (run' refWorld x)

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
  run' :: WorldId
       -> StateT ServerState IO (Maybe (Time Duration System))
       -> StateT ServerState IO RunResult
  run' refWid act =
    go >>= \res -> do
      case res of
        NotExecutedTryAgainLater _ -> stopMusic
        NotExecutedGameCanceled -> stopMusic
        Executed _ -> return ()
      return res

   where
    stopMusic = get >>= \(ServerState _ _ _ _ _ _ _ _ _ game) ->
      tryTakeMVar game >>= maybe
        (return ())
        (\g@(CurrentGame _ _ _ s) -> do
          let (newScore, notesChanges) = stopScore s
          case notesChanges of
            [] -> return ()
            _:_ -> notifyPlayersN $ map (flip PlayMusic SineSynth) notesChanges
          putMVar game $ g{score = newScore})

    go = get >>= \(ServerState _ _ _ _ _ _ _ _ terminate game) ->
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

  initializePlayers :: StateT ServerState IO (Maybe (Time Duration System))
  initializePlayers = liftIO getSystemTime >>= \start -> do
    -- we add one second to take into account the fact that start of game is delayed by one second.
    adjustAll $ \c -> c { getShipSafeUntil = Just $ addDuration (fromSecs 6) start }
    return Nothing

  stepWorld :: Time Point System -> StateT ServerState IO (Maybe (Time Duration System))
  stepWorld now = do
    let !zero = zeroCoords
        mult = initalGameMultiplicator
    accs <- Map.mapMaybe
      (\p ->
          let !acc = getShipAcceleration p
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
      get >>= \s ->
        liftIO $ modifyMVar
          (scheduledGame s)
          (\g -> let (newScore, noteChange) = stepScore $ score g in return (g {score = newScore}, noteChange))

    updateSafeShips =
      state $ \s ->
        let clients = getClients s
            (clientsMadeSafe, c') =
              Map.mapAccumWithKey
                (\shipsMadeSafe sid client -> maybe
                  (shipsMadeSafe, client)
                  (\shipUnsafeAt ->
                      if shipUnsafeAt < now
                        then
                          (Set.insert sid shipsMadeSafe, client { getShipSafeUntil = Nothing } )
                        else
                          (shipsMadeSafe, client))
                  $ getShipSafeUntil client)
                Set.empty $ getClients' clients
        in (clientsMadeSafe, s { getClients = clients { getClients' = c' } })

data RunResult =
    NotExecutedGameCanceled
  | NotExecutedTryAgainLater !(Time Duration System)
  -- ^ withe the duration to sleep before retrying
  | Executed !(Maybe (Time Duration System))
  -- ^ With an optional duration to wait before the next iteration

adjustClient :: (Client -> Client) -> ClientHandlerIO ()
adjustClient f = do
  i <- lift $ asks shipId
  modify' $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.adjust f i $ getClients' clients
                    }
          }

{-# INLINABLE adjustAll #-}
adjustAll :: (MonadState ServerState m) => (Client -> Client) -> m ()
adjustAll f =
  modify' $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.map f $ getClients' clients
                    }
          }

setClients :: (MonadState ServerState m) => Map ShipId Client -> m ()
setClients newClients =
  modify' $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' = newClients
                    }
          }

adjustAll' :: (MonadState ServerState m) => (Client -> Maybe Client) -> m [ShipId]
adjustAll' f =
  state $ \s ->
    let clients = getClients s
        m = getClients' clients
        changed = Map.mapMaybe f m
        newM = Map.union changed m -- left biased, so new elements will delete old ones.
    in (Map.keys changed
      , s { getClients = clients { getClients' = newM } })

unAssign :: (MonadIO m, MonadState ServerState m)
         => ShipId
         -> m ()
unAssign sid = gets worldCreation >>= \wc -> case creationState wc of
  Created -> return ()
  CreationAssigned assignees -> do
    let s1 = Set.size assignees
        newAssignees = Set.delete sid assignees
        s2 = Set.size newAssignees
    unless (s1 == s2) $
      modify' $ \s -> s { worldCreation = wc { creationState = CreationAssigned newAssignees } }

cancelWorldRequest :: (MonadIO m, MonadState ServerState m)
                   => m ()
cancelWorldRequest = gets worldCreation >>= \wc@(WorldCreation st wid _ _) -> case st of
  Created -> return ()
  CreationAssigned assignees -> unless (Set.null assignees) $ do
    gets clientsMap >>= notifyN [WorldRequest wid Cancel] . flip Map.restrictKeys assignees
    modify' $ \s -> s { worldCreation = wc { creationState = CreationAssigned Set.empty } }


addStats :: Map Properties Statistics
         -> WorldId
         -- ^ if this 'WorldId' doesn't match with the 'WorldId' of 'worldCreation',
         -- nothing is done because it is obsolete (eventhough the server cancels obsolete requests,
         -- this case could occur if the request was cancelled just after the non-result was
         -- sent by the client).
         -> ClientHandlerIO ()
addStats stats key =
  gets worldCreation >>= \wc@(WorldCreation st wid _ prevStats) -> do
    let newStats = safeMerge mergeStats prevStats stats
    bool
      (serverLog $ pure $ colored ("Obsolete key " <> pack (show key)) blue)
      (case st of
          -- drop newStats if world is already created.
        Created -> return ()
        CreationAssigned _ -> do
          log $ colored (pack $ show newStats) white
          modify' $ \s -> s { worldCreation = wc { creationStatistics = newStats } })
      $ wid == key


participateToWorldCreation :: WorldId
                           -- ^ if this 'WorldId' doesn't match with the 'WorldId' of 'worldCreation',
                           -- nothing is done because it is obsolete (eventhough the server cancels obsolete requests,
                           -- this case could occur if the request was cancelled just after the non-result was
                           -- sent by the client).
                           -> ClientHandlerIO ()
participateToWorldCreation key = asks shipId >>= \origin ->
  gets worldCreation >>= \wc@(WorldCreation st wid _ _) ->
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
          modify' $ \s -> s { worldCreation = wc { creationState = CreationAssigned newAssignees
                                                 } }
          gets clientsMap >>= requestWorldBy . flip Map.restrictKeys single)
      $ wid == key

requestWorld :: (MonadIO m, MonadState ServerState m)
             => m ()
requestWorld = do
  cancelWorldRequest
  modify' $ \s@(ServerState _ _ _ level params creation _ _ _ _) ->
    let nextWid = succ $ creationKey creation
    in s { worldCreation = WorldCreation
            (CreationAssigned $ Map.keysSet $ clientsMap s)
            nextWid
            (WorldSpec level (onlyPlayersIds s) params)
            Map.empty
        }
  gets clientsMap >>= requestWorldBy

requestWorldBy :: (MonadIO m, MonadState ServerState m)
               => Map ShipId Client -> m ()
requestWorldBy x =
  gets worldCreation >>= \(WorldCreation _ wid spec _) ->
    notifyN [WorldRequest wid $ Build (fromSecs 1) spec] x

onDelta :: (MonadIO m, MonadState ServerState m)
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

onChangeWorldParams :: (MonadIO m, MonadState ServerState m)
                    => (WorldParameters -> Maybe WorldParameters)
                    -> m ()
onChangeWorldParams f =
  state (\s ->
    let mayNewParams = f prevParams
        prevParams = worldParameters s
    in (mayNewParams
      , maybe s (\newParams -> s { worldParameters = newParams }) mayNewParams))
    >>= maybe (return ()) onChange
 where
  onChange p = do
    notifyEveryone $ OnWorldParameters p
    requestWorld

changeWorldShape :: WorldShape -> WorldParameters -> Maybe WorldParameters
changeWorldShape d p =
  bool (Just $ p { worldShape = d }) Nothing $ d == worldShape p

-- Game Timing:
{-
{-# INLINABLE onMove #-}
onMove :: (MonadState AppState m, MonadIO m)
       => Map ShipId (Coords Vel)
       -> [ShipId]
       -> m ()
onMove accelerations shipsLosingArmor = getGameState >>= \(GameState _ m@(Multiplicator mv) world a b c d e) ->
  liftIO getSystemTime >>= \t -> do
    let nextTime = addDuration (toSystemDuration m gameMotionPeriod) t
    putGameState $ GameState (Just nextTime) (Multiplicator (mv + 0.01)) (moveWorld accelerations shipsLosingArmor world) a b c d e
    onHasMoved t
-}
