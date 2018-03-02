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

import           Imj.Prelude hiding(intercalate)
import qualified Imj.Prelude as Prel (intercalate)

import           Control.Concurrent(threadDelay, forkIO)
import           Control.Monad.IO.Unlift(MonadUnliftIO, MonadIO)
import           Control.Monad.Reader(runReaderT, lift, asks)
import           Control.Monad.State.Strict(StateT, MonadState, runStateT, execStateT, modify', get, gets, state)
import           Data.Char (isPunctuation, isSpace, toLower)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map(map, mapMaybe, union, filter, take, elems, keys, adjust, keysSet
                                      , insert, null, empty, mapAccumWithKey, updateLookupWithKey
                                      , restrictKeys, traverseWithKey, lookup, toList)
import           Data.Set (Set)
import qualified Data.Set as Set (difference, lookupMin, empty, insert)
import           Data.Maybe(isJust)
import           Data.Text(pack, unpack, justifyRight)
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
                                , readMVar, tryReadMVar, takeMVar, putMVar) -- to communicate between client handlers and game scheduler

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Class.ClientNode
import           Imj.Graphics.Color.Types

import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Geo.Discrete(translateInDir, zeroCoords)
import           Imj.Graphics.Text.ColorString(ColorString, intercalate, colored)
import           Imj.Graphics.Color
import           Imj.Log

type ClientHandlerIO = StateT ServerState (ReaderT ConstClient IO)

-- | Data of a 'Client' that will never change.
data ConstClient = ConstClient {
    connection :: {-# UNPACK #-} !Connection
  , shipId :: {-# UNPACK #-} !ShipId
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

mkServer :: Maybe ColorScheme -> Maybe ServerLogs -> Maybe ServerName -> ServerPort -> Server
mkServer color logs Nothing =
  Local (fromMaybe NoLogs logs) (fromMaybe (ColorScheme $ rgb 3 2 2) color)
mkServer Nothing Nothing (Just (ServerName n)) =
  Distant $ ServerName $ map toLower n
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

{-# INLINABLE notifyPlayers #-}
notifyPlayers :: (MonadIO m, MonadState ServerState m) => ServerEvent -> m ()
notifyPlayers evt =
  notifyN [evt] =<< gets onlyPlayersMap

notifyFirstWorldBuilder :: (MonadIO m, MonadState ServerState m) => ServerEvent -> m ()
notifyFirstWorldBuilder evt =
  notifyN [evt] . Map.take 1 =<< gets clientsMap

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
  -- we do a special case for Connect. But ideally we should be abble to disconnect a client.
  -- and reconnect it in the loop. To do that, when we disconnect a client, it will not
  -- be removed from the map, instead its state will be Disconnected. (TODO)
  receiveData conn >>= \case
    Connect sn@(SuggestedPlayerName suggestedName) cliType -> either
      (sendBinaryData conn . ConnectionRefused . InvalidName sn)
      (\_ -> modifyMVar st (fmap swap . runStateT takeShipId) >>=
               runReaderT (handleClient st sn cliType) . ConstClient conn)
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
    modifyMVar_ st $ execStateT $ addClient sn cliType
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

takeShipId :: StateT ServerState IO ShipId
takeShipId =
  takeDisconnectedShipId >>= maybe
    (state $ \s ->
      let newShipId = getNextShipId $ getClients s
      in (newShipId, s { getClients = (getClients s) { getNextShipId = succ newShipId } } ))
    (\(gameConnectedPlayers, disconnected) -> do
      if Map.null gameConnectedPlayers
        then
          serverError "the current game has no connected player" -- TODO support that
        else do
          serverLog $ ("Reconnecting using " <>) <$> showId disconnected
          notifyN [CurrentGameStateRequest] (Map.take 1 gameConnectedPlayers)
      return disconnected)
 where
  takeDisconnectedShipId :: StateT ServerState IO (Maybe (Map ShipId Client, ShipId)) -- (connected, disconnected)
  takeDisconnectedShipId =
    get >>= \(ServerState _ _ _ _ _ _ _ _ terminate game) ->
      if terminate
        then
          return Nothing
        else
          liftIO (tryReadMVar game) >>= maybe
            (do serverLog $ pure "No game is running"
                return Nothing)
            (\(CurrentGame _ gamePlayers status) -> case status of
                (Paused _) -> do
                -- we /could/ use 'Paused' ignored argument but it's probably out-of-date
                -- - the game scheduler thread updates it every second only -
                -- so we recompute the difference:
                  connectedPlayers <- gets onlyPlayersMap
                  let connectedPlayerKeys = Map.keysSet connectedPlayers
                      mayDisconnectedPlayerKey = Set.lookupMin $ Set.difference gamePlayers connectedPlayerKeys
                      gameConnectedPlayers = Map.restrictKeys connectedPlayers gamePlayers
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
    gets clientsMap
  notifyClient $ ConnectionAccepted i $
    Map.map
      (\(Client n _ _ _ _ _ _ color) -> PlayerEssence n Present color)
      presentClients

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
  any ((== name) . getName) <$> gets allClients >>= \case
    True  -> return $ Left "Name is already taken"
    False -> return $ Right ()

shutdown :: Text -> StateT ServerState IO ()
shutdown reason = do
  serverLog $ pure $ colored "Server shutdown" red <> "|" <> colored reason (rgb 4 1 1)
  modify' $ \s -> s { shouldTerminate = True }
  mapM_ (disconnect $ ServerShutdown reason) =<< gets allClientsIds

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

disconnectClient :: ClientHandlerIO ()
disconnectClient = do
  sid <- asks shipId
  disconnect ClientShutdown sid

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
                -- Because we pass a 'ServerShutdown', 'disconnectClient' won't
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
    -- notify other clients about the disconnection of client
    case reason of
      BrokenClient e -> notifyEveryone $ RunCommand cid $ Leaves $ ConnectionError e
      ClientShutdown -> notifyEveryone $ RunCommand cid $ Leaves Intentional
      ServerShutdown _ -> return () -- no need to notify other clients, as they will be diconnected too,
                                    -- hence they will receive a server shutdown notification.
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

{-# INLINE allClients #-}
allClients :: ServerState -> [Client]
allClients = Map.elems . clientsMap

{-# INLINE clientsMap #-}
clientsMap :: ServerState -> Map ShipId Client
clientsMap = getClients' . getClients

allClientsIds :: ServerState -> [ShipId]
allClientsIds = Map.keys . clientsMap

onlyPlayersMap :: ServerState -> Map ShipId Client
onlyPlayersMap = Map.filter (isJust . getState) . clientsMap

onlyPlayers :: ServerState -> [Client]
onlyPlayers = filter (isJust . getState) . allClients

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
  notifyClient $ Error txt
  error txt
 where
  txt = Prel.intercalate "|" [from, "error from Server", msg]

serverError :: String -> StateT ServerState IO ()
serverError msg = do
  serverLog $ pure $ colored (pack txt) red
  notifyEveryone $ Error txt
  error txt
 where
  txt = Prel.intercalate "|" ["Server error from Server", msg]

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
  RequestCommand cmd@(AssignName name) -> either
    (notifyClient . CommandError cmd)
    (\_ -> checkNameAvailability name >>= either
      (notifyClient . CommandError cmd)
      (\_ -> do
        adjustClient $ \c -> c { getName = name }
        acceptCmd cmd)
    ) $ checkName name
  RequestCommand cmd@(AssignColor _) ->
    notifyClient $ CommandError cmd "use SetColorSchemeCenter instead"
  Report TellColorSchemeCenter ->
    colorSchemeCenterStr >>= notifyClient . Reporting TellColorSchemeCenter
  Do cmd@(SetColorSchemeCenter color) -> do
    modify' $ \s -> s { centerColor = color }
    gets clientsMap >>= Map.traverseWithKey (\i c -> do
      col <- mkClientColor i
      return c { getColor = col })
        >>= \newClients -> do
          setClients newClients
          notifyEveryoneN $
            map (\(k, c) -> RunCommand k (AssignColor $ getColor c)) $
            Map.toList newClients
    colorSchemeCenterStr >>= publish . Done cmd
  RequestCommand cmd@(Says _) ->
    acceptCmd cmd
  RequestCommand (Leaves _) ->
    disconnectClient -- will do the corresponding 'notifyEveryone $ RunCommand'
  ExitedState Excluded -> gets intent >>= \case
    IntentSetup -> do
      -- change client state to make it playable
      adjustClient $ \c -> c { getState = Just Finished }
      publish Joins
      -- request world to let the client have a world
      requestWorld
      -- next step when client 'IsReady'
    _ -> do
      notifyClient $ EnterState Excluded
      publish WaitsToJoin
  ExitedState (PlayLevel _) -> return ()
  LevelEnded outcome -> do
    adjustClient $ \c -> c { getState = Just Finished }
    gets intent >>= \case
      IntentPlayGame ->
        modify' $ \s -> s { intent = IntentLevelEnd outcome }
      IntentLevelEnd o ->
        if o == outcome
          then
            return ()
          else
            gameError $ "inconsistent outcomes:" ++ show (o, outcome)
      IntentSetup ->
        gameError "LevelEnded received while in IntentSetup"
    gets onlyPlayers >>= \players' -> do
      let playersAllFinished = all (finished . getState) players'
          finished Nothing = error "should not happen"
          finished (Just Finished) = True
          finished (Just InGame) = False
      when playersAllFinished $ do
        onLevelOutcome outcome
        requestWorld
------------------------------------------------------------------------------
-- Clients in 'Setup' state can configure the world.
------------------------------------------------------------------------------
  ChangeWallDistribution t ->
    onChangeWorldParams $ changeWallDistrib t
  ChangeWorldShape s ->
    onChangeWorldParams $ changeWorldShape s
  ExitedState Setup -> gets intent >>= \case
    IntentSetup -> do
      modify' $ \s -> s { intent = IntentPlayGame }
      publish StartsGame
      notifyPlayers $ ExitState Setup
      requestWorld
    _ -> return ()
  WorldProposal essence ->
    get >>= \(ServerState _ _ _ levelSpec _ lastWid _ _ _ _) -> do
      let wid = fromMaybe (error "Nothing WorldId in WorldProposal") $ getWorldId essence
      when (lastWid == Just wid) $
        notifyPlayers $ ChangeLevel (mkLevelEssence levelSpec) essence
  CurrentGameState gameStateEssence ->
    gets scheduledGame >>= liftIO . tryReadMVar >>= \case
      (Just (CurrentGame _ gamePlayers (Paused _))) -> do
        disconnectedPlayerKeys <- Set.difference gamePlayers <$> gets onlyPlayersIds
        flip Map.restrictKeys disconnectedPlayerKeys <$> gets clientsMap >>=
          notifyN [PutGameState gameStateEssence]
      invalid -> handlerError $ "CurrentGameState sent while game is " ++ show invalid
------------------------------------------------------------------------------
-- Clients notify when they have received a given world, and the corresponding
--   UI Animation is over (hence the level shall start, for example.):
------------------------------------------------------------------------------
  IsReady wid -> do
    adjustClient $ \c -> c { getCurrentWorld = Just wid }
    gets intent >>= \case
      IntentSetup ->
        -- (follow-up from 'ExitedState Excluded')
        -- Allow the client to setup the world, now that the world contains its ship.
        notifyClient $ EnterState Setup
      IntentPlayGame ->
        get >>= \(ServerState _ (Clients clients _) _ _ _ mayLastWId _ _ _ game) -> maybe
            (return ())
            (\lastWId -> do
              adjustClient $ \c -> c { getState = Just Finished }
              -- start the game when all players have the right world
              playersKeys <- gets onlyPlayersIds
              let playersAllReady =
                    all ((== Just lastWId) . getCurrentWorld) $ Map.restrictKeys clients playersKeys
              when playersAllReady $ do
                adjustAll $ \c ->
                  if getState c == Just Finished
                    then c { getState = Just InGame
                           , getShipAcceleration = zeroCoords }
                    else c
                liftIO (tryReadMVar game) >>= maybe
                  -- non blocking because all game changes are done inside a modifyMVar
                  -- of 'ServerState' and we are inside one.
                  (void $ liftIO $ putMVar game $ mkCurrentGame lastWId playersKeys)
                  (\(CurrentGame wid' _ _) -> -- reconnection scenario
                      when (wid' /= lastWId) $
                        handlerError $ "reconnection failed " ++ show (wid', lastWId)))
            mayLastWId

      IntentLevelEnd _ -> return ()
------------------------------------------------------------------------------
-- Clients in 'PlayLevel' state can play the game.
------------------------------------------------------------------------------
  -- Due to network latency, laser shots may be applied to a world state
  -- different from what the player saw when the shot was made.
  -- But since the laser shot will be rendered with latency, too, the player will be
  -- able to integrate the latency via this visual feedback - I read somewhere that
  -- latency is not a big problem for players, provided that it is stable over time.
  Action Laser dir ->
    lift (asks shipId) >>= notifyPlayers . GameEvent . LaserShot dir
  Action Ship dir ->
    adjustClient $ \c -> c { getShipAcceleration = translateInDir dir $ getShipAcceleration c }
 where
  publish a = lift (asks shipId) >>= notifyEveryone . PlayerInfo a
  acceptCmd cmd = lift (asks shipId) >>= notifyEveryone . flip RunCommand cmd
  colorSchemeCenterStr = ("Color scheme center is:" <>) . pack . show . color8CodeToXterm256 <$> gets centerColor


onLevelOutcome :: (MonadIO m, MonadState ServerState m)
               => LevelOutcome -> m ()
onLevelOutcome outcome = do
  gets scheduledGame >>= void . liftIO . takeMVar -- to stop the scheduler
  levelN <- levelNumber <$> gets levelSpecification
  notifyEveryoneN $
    (GameInfo $ LevelResult levelN outcome):
    [GameInfo GameWon | outcome == Won && levelN == lastLevel]
  if outcome == Won && levelN < lastLevel
    then
      onNextLevel levelN
    else
      onGameEnd

onNextLevel :: MonadState ServerState m
            => Int -> m ()
onNextLevel levelN =
  modify' $ \s -> s { levelSpecification = (levelSpecification s) { levelNumber = succ levelN }
                  , intent = IntentPlayGame
                  }

onGameEnd :: (MonadIO m, MonadState ServerState m)
          => m ()
onGameEnd = do
  modify' $ \s -> s { levelSpecification = (levelSpecification s) { levelNumber = firstServerLevel }
                    , intent = IntentSetup
                    }
  -- allow fresh clients to become players
  adjustAll' (\c ->
    maybe
      (Just $ c { getState = Just Finished })
      (const Nothing)
      $ getState c)
      >>= notifyEveryoneN . map (PlayerInfo Joins)

gameScheduler :: MVar ServerState -> IO ()
gameScheduler st =
  readMVar st >>= \(ServerState _ _ _ _ _ _ _ _ terminate mayGame) ->
    if terminate
      then return ()
      else
        -- block until 'scheduledGame' contains a 'CurrentGame'
        readMVar mayGame >>= \(CurrentGame refWorld _ _) -> do
          let run x = modifyMVar st $ fmap swap . runStateT (run' refWorld x)
              waitForPlayerReconnection = threadDelay $ fromIntegral $ toMicros $ fromSecs 1
              toActOrNotToAct act continuityArg continuation = act >>= \case
                Executed ->
                  continuation continuityArg
                NotExecutedTryAgainLater -> do
                  waitForPlayerReconnection
                  -- retry, but forget the continuityArg as here we waited for reconnection,
                  -- which introduced a discontinuity.
                  toActOrNotToAct act Nothing continuation
                NotExecutedGameCanceled ->
                  return ()
          toActOrNotToAct (run initializePlayers) Nothing $ \_ -> do
            let mult = initalGameMultiplicator
                go mayPrevUpdate = do
                  now <- getSystemTime
                  let baseTime = fromMaybe now mayPrevUpdate
                      update = addDuration (toSystemDuration mult gameMotionPeriod) baseTime
                  threadDelay $ fromIntegral $ toMicros $ now...update
                  toActOrNotToAct (run (stepWorld now)) (Just update) go
            go Nothing
          gameScheduler st
 where
  initializePlayers :: StateT ServerState IO ()
  initializePlayers = liftIO getSystemTime >>= \start ->
    -- we add one second to take into account the fact that start of game is delayed by one second.
    adjustAll $ \c -> c { getShipSafeUntil = Just $ addDuration (fromSecs 6) start }

  stepWorld :: Time Point System -> StateT ServerState IO ()
  stepWorld now = do
    let !zero = zeroCoords
    accs <- Map.mapMaybe
      (\p ->
          let !acc = getShipAcceleration p
            in if acc == zero
              then Nothing
              else Just acc)
      <$> gets onlyPlayersMap
    becameSafe <- updateSafeShips
    notifyPlayers $ GameEvent $ PeriodicMotion accs becameSafe
    adjustAll $ \p -> p { getShipAcceleration = zeroCoords }
   where
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

  -- run the action if the world matches the current world and the game was
  -- not terminated
  run' :: WorldId -> StateT ServerState IO () -> StateT ServerState IO RunResult
  run' refWid act = get >>= \(ServerState _ _ _ _ _ _ _ _ terminate mayWorld) ->
    if terminate
      then do
        serverLog $ pure $ colored "Terminating game" yellow
        return NotExecutedGameCanceled
      else
        -- we use 'tryReadMVar' to /not/ block here, as we are inside a modifyMVar.
        liftIO (tryReadMVar mayWorld) >>= maybe
          (return NotExecutedGameCanceled)
          (\(CurrentGame curWid gamePlayers status) ->
            if refWid /= curWid
              then do
                serverLog $ pure $ colored ("The world has changed: " <> pack (show (curWid, refWid))) yellow
                return NotExecutedGameCanceled -- the world has changed
              else do
                connectedPlayers <- gets onlyPlayersIds
                let missingPlayers = Set.difference gamePlayers connectedPlayers
                    newStatus
                      | null connectedPlayers = CancelledNoConnectedPlayer
                      | null missingPlayers = Running
                      | otherwise = Paused missingPlayers
                    statusChanged = status /= newStatus
                when statusChanged $ do
                    serverLog $ pure $ colored ("Game status change: " <> pack (show (status, newStatus))) yellow

                    -- mayWorld can be concurrently modified from client handler threads, but only from
                    -- inside a 'modifyMVar' on 'ServerState'. Since we are ourselves inside
                    -- a 'modifyMVar' of 'ServerState', we guarantee that the 'takeMVar' inside
                    -- 'swapMVar' won't block, because in the same 'modifyMVar' transaction, 'tryReadMVar'
                    -- returned a 'Just'.
                    liftIO $ void $ swapMVar mayWorld $ CurrentGame curWid gamePlayers newStatus
                    notifyPlayers $ EnterState $ PlayLevel newStatus
                case newStatus of
                  New -> serverError "logic : newStatus == New" >> return NotExecutedTryAgainLater
                  Paused _ ->
                    return NotExecutedTryAgainLater
                  CancelledNoConnectedPlayer -> do
                    onLevelOutcome $ Lost "All players left"
                    return NotExecutedGameCanceled
                  Running ->
                    if statusChanged
                      then
                        return NotExecutedTryAgainLater
                      else
                        act >> return Executed)

data RunResult =
    NotExecutedGameCanceled
  | NotExecutedTryAgainLater
  | Executed

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

requestWorld :: (MonadIO m, MonadState ServerState m) => m ()
requestWorld = do
  incrementWorldId
  get >>= \(ServerState _ _ _ level params wid _ _ _ _) -> do
    shipIds <- gets onlyPlayersIds
    notifyFirstWorldBuilder $ WorldRequest $ WorldSpec level shipIds params wid
 where
  incrementWorldId =
    modify' $ \s ->
      let wid = maybe (WorldId 0) succ $ lastRequestedWorldId s
      in s { lastRequestedWorldId = Just wid }


onChangeWorldParams :: (MonadIO m, MonadState ServerState m)
                    => (WorldParameters -> WorldParameters)
                    -> m ()
onChangeWorldParams f = do
  modify' $ \s ->
    s { worldParameters = f $ worldParameters s }
  requestWorld

changeWallDistrib :: WallDistribution -> WorldParameters -> WorldParameters
changeWallDistrib d p = p { wallDistrib = d }

changeWorldShape :: WorldShape -> WorldParameters -> WorldParameters
changeWorldShape d p = p { worldShape = d }

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
