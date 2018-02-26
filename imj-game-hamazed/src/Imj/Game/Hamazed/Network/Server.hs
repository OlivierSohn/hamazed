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

import           Imj.Prelude hiding(intercalate, concat)
import qualified Imj.Prelude as Prel (intercalate)

import           Control.Concurrent(threadDelay, forkIO)
import           Control.Concurrent.MVar (MVar
                                        , modifyMVar_, modifyMVar, swapMVar
                                        , readMVar, tryReadMVar, takeMVar, putMVar) -- to communicate between client handlers and game scheduler
import           Control.Monad.Reader(runReaderT, lift, asks)
import           Control.Monad.State.Strict(StateT, runStateT, execStateT, modify', get, gets, state)
import           Data.Char (isPunctuation, isSpace, toLower)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map(map, mapMaybe, union, filter, take, elems, keys, adjust, keysSet
                                      , insert, null, empty, mapAccumWithKey, updateLookupWithKey
                                      , restrictKeys, traverseWithKey, lookup)
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

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Class.ClientNode

import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Geo.Discrete(translateInDir, zeroCoords)
import           Imj.Graphics.Text.ColorString(ColorString, intercalate, colored)
import           Imj.Graphics.Color
import           Imj.Log

type ClientHandlerIO = ReaderT ConstClient (StateT ServerState IO)

-- | Data of a 'Client' that will never change.
data ConstClient = ConstClient {
    shipId :: {-# UNPACK #-} !ShipId
  , connection :: {-# UNPACK #-} !Connection -- TODO remove from 'Client', and use ConstClient as key in Map
}

log :: ColorString -> ClientHandlerIO ()
log msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs -> do
    i <- asks shipId
    idStr <- lift $ showId i
    liftIO $ baseLog $ intercalate "|"
      [ idStr
      , msg
      ]

showId :: ShipId -> StateT ServerState IO ColorString
showId i = do
  color <- fromMaybe white <$> gets (clientColor i)
  return $ colored (pack $ show i) color

showClient :: Client -> ColorString
showClient c = colored (pack $ show c) $ gray 16

serverLog :: StateT ServerState IO ColorString
          -> StateT ServerState IO ()
serverLog msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs ->
    msg >>= liftIO . baseLog

defaultPort :: ServerPort
defaultPort = ServerPort 10052

mkServer :: Maybe ServerLogs -> Maybe ServerName -> ServerPort -> Server
mkServer logs Nothing =
  Local (fromMaybe NoLogs logs)
mkServer Nothing (Just (ServerName n)) =
  Distant $ ServerName $ map toLower n
mkServer (Just _) (Just _) =
  error "'--serverLogs' conflicts with '--serverName' (these options are mutually exclusive)."

notifyEveryone :: ServerEvent -> StateT ServerState IO ()
notifyEveryone evt =
  notifyN [evt] =<< gets clientsMap

notifyEveryoneN :: [ServerEvent] -> StateT ServerState IO ()
notifyEveryoneN evts =
  notifyN evts =<< gets clientsMap

notifyPlayers :: ServerEvent -> StateT ServerState IO ()
notifyPlayers evt =
  notifyN [evt] =<< gets onlyPlayersMap

notifyFirstWorldBuilder :: ServerEvent -> StateT ServerState IO ()
notifyFirstWorldBuilder evt =
  notifyN [evt] . Map.take 1 =<< gets clientsMap

{-# INLINABLE notifyN #-}
-- | Uses sendDataMessage which is at a lower-level than sendBinaryData
-- to factorize serialization.
--
-- In case some connections are closed, the corresponding clients are silently removed
-- from the Map, and we continue the processing.
notifyN :: [ServerEvent]
        -> Map ShipId Client
        -> StateT ServerState IO ()
notifyN evts =
  void . Map.traverseWithKey
    (\i client -> sendAndHandleExceptions evts (getConnection client) i)

notify :: Connection -> ShipId -> ServerEvent -> StateT ServerState IO ()
notify conn sid evt =
  sendAndHandleExceptions [evt] conn sid

notifyClient :: ServerEvent -> ClientHandlerIO ()
notifyClient evt = do
  conn <- asks connection
  sid <- asks shipId
  lift $ notify conn sid evt

sendAndHandleExceptions :: [ServerEvent] -> Connection -> ShipId -> StateT ServerState IO ()
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
      (\_ -> do
        i <- modifyMVar st (fmap swap . runStateT takeShipId)
        let env = ConstClient i conn
            disconnectOnException name action = try action >>= either
              (\(e :: SomeException) ->
                  modifyMVar_ st $ execStateT $
                    onBrokenClient name (Nothing :: Maybe (Text, Text, [Text])) e i)
              return
        -- To detect disconnections when communication is idle:
        void $ forkIO $ disconnectOnException "PingPong" $ pingPong conn $ fromSecs 1
        disconnectOnException "Handler" $ do
          modifyMVar_ st $ execStateT $ flip runReaderT env $ addClient sn cliType
          forever $ receiveData conn >>=
            modifyMVar_ st . execStateT . flip runReaderT env . handleIncomingEvent
            -- hereabove I runReaderT within execStateT.
            -- TODO runReaderT globally, from the moment we have i and conn.
      ) $ checkName $ PlayerName $ pack suggestedName
    msg -> error $ "First sent message should be 'Connect'. " ++ show msg

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
      let newShipId = succ $ getNextShipId $ getClients s
      in ( newShipId, s { getClients = (getClients s) { getNextShipId = newShipId } } ))
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
  playerColor <- mkColor
  presentClients <- lift $ do
    name <- makePlayerName sn
    let client@(Client _ _ _ _ _ _ _ c) = mkClient name playerColor conn cliType
    serverLog $ pure $ colored "Adding client " green <> showClient client
    -- this call is /before/ addClient to avoid sending redundant info to client.
    notifyEveryoneN $ map (RunCommand i) [AssignName name, AssignColors c]
    -- order matters, see comment above.
    modify' $ \ s ->
      let clients = getClients s
      in s { getClients =
              clients { getClients' =
                Map.insert i client $ getClients' clients } }
    gets clientsMap
  notifyClient $ ConnectionAccepted i $
    Map.map
      (\(Client n _ _ _ _ _ _ color) -> Player n Present color)
      presentClients
 where
  mkColor = do
    t <- gets startSecond
    (ShipId i) <- asks shipId
    let !ref = rgb 3 2 0
        nColors = countHuesOfSameIntensity ref
        n = (t + fromIntegral i) `mod` nColors
    return $ rotateHue (fromIntegral n / fromIntegral nColors) ref

makePlayerName :: SuggestedPlayerName -> StateT ServerState IO PlayerName
makePlayerName (SuggestedPlayerName sn) = do
  let go mayI = do
        let proposal = PlayerName $ pack $ maybe sn ((++) sn . show) mayI
        checkNameAvailability proposal >>= either
          (\_ -> go $ Just $ maybe (2::Int) succ mayI)
          (\_ -> return proposal)
  go Nothing

checkNameAvailability :: PlayerName -> StateT ServerState IO (Either Text ())
checkNameAvailability name =
  any ((== name) . getName) <$> gets allClients >>= \case
    True  -> return $ Left "Name is already taken"
    False -> return $ Right ()

shutdown :: Text -> StateT ServerState IO ()
shutdown reason = do
  modify' $ \s -> s { shouldTerminate = True }
  mapM_ (disconnect $ ServerShutdown reason) =<< gets allClientsIds

-- It's important that this function doesn't throw any exception.
onBrokenClient :: (Show a)
               => Text
               -- ^ Describes the type of thread in which the exception was raised
               -> Maybe (Text, Text, [a])
               -- ^ A list of values that will be added to the log.
               -> SomeException
               -> ShipId
               -> StateT ServerState IO ()
onBrokenClient threadCategory infos e i = do
  log'
  disconnect (BrokenClient $ pack $ show e) i
 where
  log' = serverLog $
    showId i >>= \strId -> pure $ firstLine strId <> logDetailedException infos e
  firstLine s = intercalate "|"
    [ colored "BrokenClient" red
    , colored (justifyRight 10 '.' threadCategory) yellow
    , s
    ]

disconnectClient :: ClientHandlerIO ()
disconnectClient = do
  sid <- asks shipId
  lift $ disconnect ClientShutdown sid

disconnect :: DisconnectReason -> ShipId -> StateT ServerState IO ()
disconnect r i =
  tryRemoveClient >>= maybe
    (do serverLog $ (\s -> "The client " <> s <> " was already removed.") <$> showId i
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
  closeConnection :: DisconnectReason -> ShipId -> Client -> StateT ServerState IO ()
  closeConnection reason cid c@(Client (PlayerName playerName) conn _ _ _ _ _ _) = do
    serverLog $ pure $ colored "Closing connection of " yellow <> showClient c
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
clientColor i = fmap (getPlayerColor . getColors) . findClient i

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
    (\_ -> lift (checkNameAvailability name) >>= either
      (notifyClient . CommandError cmd)
      (\_ -> do
        adjustClient $ \c -> c { getName = name }
        acceptCmd cmd)
    ) $ checkName name
  RequestCommand cmd@(AssignColors colors) -> do
    adjustClient $ \c -> c { getColors = colors }
    acceptCmd cmd
  RequestCommand cmd@(Says _) ->
    acceptCmd cmd
  RequestCommand (Leaves _) ->
    disconnectClient -- will do the corresponding 'notifyEveryone $ RunCommand'
  ExitedState Excluded ->
    gets intent >>= \case
      IntentSetup -> do
        -- change client state to make it playable
        adjustClient $ \c -> c { getState = Just Finished }
        asks shipId >>= \i -> lift $ do
          notifyEveryone $ PlayerInfo Joins i
          -- request world to let the client have a world
          requestWorld
          -- next step when client 'IsReady'
      _ -> do
        notifyClient $ EnterState Excluded
        asks shipId >>= lift . notifyEveryone . PlayerInfo WaitsToJoin
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
    lift $ gets onlyPlayers >>= \players -> do
      let playersAllFinished = all (finished . getState) players
          finished Nothing = error "should not happen"
          finished (Just Finished) = True
          finished (Just InGame) = False
      when playersAllFinished $ gets scheduledGame >>= void . liftIO . takeMVar >> do
        levelN <- levelNumber <$> gets levelSpecification
        notifyEveryoneN $
          (GameInfo $ LevelResult levelN outcome):
          [GameInfo GameWon | outcome == Won && levelN == lastLevel]
        if outcome == Won && levelN < lastLevel
          then
            modify' $ \s -> s { levelSpecification = (levelSpecification s) { levelNumber = succ levelN }
                              , intent = IntentPlayGame
                              }
          else do
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
        requestWorld
------------------------------------------------------------------------------
-- Clients in 'Setup' state can configure the world.
------------------------------------------------------------------------------
  ChangeWallDistribution t ->
    lift $ onChangeWorldParams $ changeWallDistrib t
  ChangeWorldShape s ->
    lift $ onChangeWorldParams $ changeWorldShape s
  ExitedState Setup -> gets intent >>= \case
    IntentSetup -> do
      modify' $ \s -> s { intent = IntentPlayGame }
      i <- asks shipId
      lift $ do
        notifyEveryone $ PlayerInfo StartsGame i
        notifyPlayers $ ExitState Setup
        requestWorld
    _ -> return ()
  WorldProposal essence ->
    get >>= \(ServerState _ _ _ levelSpec _ lastWid _ _ _ _) -> do
      let wid = fromMaybe (error "Nothing WorldId in WorldProposal") $ getWorldId essence
      when (lastWid == Just wid) $
        lift $ notifyPlayers $ ChangeLevel (mkLevelEssence levelSpec) essence
  CurrentGameState gameStateEssence ->
    gets scheduledGame >>= liftIO . tryReadMVar >>= \case
      (Just (CurrentGame _ gamePlayers (Paused _))) -> do
        disconnectedPlayerKeys <- Set.difference gamePlayers <$> gets onlyPlayersIds
        flip Map.restrictKeys disconnectedPlayerKeys <$> gets clientsMap >>=
          lift . notifyN [PutGameState gameStateEssence]
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
                lift $ adjustAll $ \c ->
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
    asks shipId >>= lift . notifyPlayers . GameEvent . LaserShot dir
  Action Ship dir ->
    adjustClient $ \c -> c { getShipAcceleration = translateInDir dir $ getShipAcceleration c }
 where
  acceptCmd cmd = asks shipId >>= lift . notifyEveryone . flip RunCommand cmd

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
      then
        return NotExecutedGameCanceled
      else
        -- we use 'tryReadMVar' to /not/ block here, as we are inside a modifyMVar.
        liftIO (tryReadMVar mayWorld) >>= maybe
          (return NotExecutedGameCanceled)
          (\(CurrentGame curWid gamePlayers status) ->
            if refWid /= curWid
              then
                return NotExecutedGameCanceled -- the world has changed
              else do
                missingPlayers <- Set.difference gamePlayers <$> gets onlyPlayersIds
                let newStatus =
                      if null missingPlayers
                        then
                          Running
                        else
                          Paused missingPlayers
                if status /= newStatus
                  then do
                    -- mayWorld can be concurrently modified from client handler threads, but only from
                    -- inside a 'modifyMVar' on 'ServerState'. Since we are ourselves inside
                    -- a 'modifyMVar' of 'ServerState', we guarantee that the 'takeMVar' inside
                    -- 'swapMVar' won't block, because in the same 'modifyMVar' transaction, 'tryReadMVar'
                    -- returned a 'Just'.
                    liftIO $ void $ swapMVar mayWorld $ CurrentGame curWid gamePlayers newStatus
                    notifyPlayers $ EnterState $ PlayLevel newStatus
                    return NotExecutedTryAgainLater
                  else
                    case newStatus of
                      New -> do
                        serverError "logic : newStatus == New"
                        return NotExecutedTryAgainLater
                      Paused _ ->
                        return NotExecutedTryAgainLater
                      Running -> do
                        act
                        return Executed)

data RunResult =
    NotExecutedGameCanceled
  | NotExecutedTryAgainLater
  | Executed

adjustClient :: (Client -> Client) -> ClientHandlerIO ()
adjustClient f = do
  i <- asks shipId
  modify' $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.adjust f i $ getClients' clients
                    }
          }

adjustAll :: (Client -> Client) -> StateT ServerState IO ()
adjustAll f =
  modify' $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.map f $ getClients' clients
                    }
          }

adjustAll' :: (Client -> Maybe Client) -> StateT ServerState IO [ShipId]
adjustAll' f =
  state $ \s ->
    let clients = getClients s
        m = getClients' clients
        changed = Map.mapMaybe f m
        newM = Map.union changed m -- left biased, so new elements will delete old ones.
    in (Map.keys changed
      , s { getClients = clients { getClients' = newM } })

requestWorld :: StateT ServerState IO ()
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


onChangeWorldParams :: (WorldParameters -> WorldParameters)
                    -> StateT ServerState IO ()
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
