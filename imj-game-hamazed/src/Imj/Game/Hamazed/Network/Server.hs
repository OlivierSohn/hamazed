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
import           Control.Concurrent.MVar (MVar
                                        , modifyMVar_, modifyMVar, swapMVar
                                        , readMVar, tryReadMVar, takeMVar, putMVar) -- to communicate between client handlers and game scheduler
import           Control.Monad.State.Strict(StateT, runStateT, execStateT, modify', get, state)
import           Data.Char (isPunctuation, isSpace, toLower)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map(map, mapMaybe, union, filter, take, elems, keys, adjust, keysSet
                                      , insert, null, empty, mapAccumWithKey, updateLookupWithKey
                                      , restrictKeys, traverseWithKey)
import           Data.Set (Set)
import qualified Data.Set as Set (difference, lookupMin, empty, insert)
import           Data.Maybe(isJust)
import           Data.Text(pack, unpack)
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

defaultPort :: ServerPort
defaultPort = ServerPort 10052

mkServer :: Maybe ServerName -> ServerPort -> Server
mkServer Nothing = Local
mkServer (Just (ServerName n)) =
  Distant $ ServerName $ map toLower n

sendClients :: ServerEvent -> StateT ServerState IO () -- TODO rename sendEveryone
sendClients evt =
  sendN [evt] =<< clientsMap

sendClientsN :: [ServerEvent] -> StateT ServerState IO ()
sendClientsN evts =
  sendN evts =<< clientsMap

sendPlayers :: ServerEvent -> StateT ServerState IO ()
sendPlayers evt =
  sendN [evt] =<< onlyPlayersMap

sendFirstWorldBuilder :: ServerEvent -> StateT ServerState IO ()
sendFirstWorldBuilder evt =
  sendN [evt] . Map.take 1 =<< clientsMap

{-# INLINABLE sendN #-}
-- | Uses sendDataMessage which is at a lower-level than sendBinaryData
-- to factorize serialization.
--
-- In case some connections are closed, the corresponding clients are silently removed
-- from the Map, and we continue the processing.
sendN :: WebSocketsData a
      => [a]
      -> Map ShipId Client
      -> StateT ServerState IO ()
sendN a clients =
  let !msgs = map (Binary . toLazyByteString) a
  in void $ Map.traverseWithKey (\i client -> sendAndHandleExceptions msgs (getConnection client) i) clients

send :: Connection -> ShipId -> ServerEvent -> StateT ServerState IO ()
send conn sid evt =
  sendAndHandleExceptions (map (Binary . toLazyByteString) [evt]) conn sid

sendAndHandleExceptions :: [DataMessage] -> Connection -> ShipId -> StateT ServerState IO ()
sendAndHandleExceptions m conn i =
  liftIO (try (sendDataMessages conn m)) >>= either
    (\(e :: SomeException) -> onBrokenClient e i)
    return

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
        let disconnectOnException action = try action >>= either
              (\(e :: SomeException) -> modifyMVar_ st $ execStateT $ onBrokenClient e i)
              return
        -- To detect disconnections when communication is idle:
        void $ forkIO $ disconnectOnException $ pingPong conn $ fromSecs 1
        disconnectOnException $ do
          modifyMVar_ st (execStateT (addClient i conn sn cliType))
          forever $ receiveData conn >>= modifyMVar_ st . execStateT . handleIncomingEvent conn i
      ) $ checkName $ PlayerName $ pack suggestedName
    msg -> error $ "first sent message should be 'Connect'. " ++ show msg

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
        else
          sendN [CurrentGameStateRequest] (Map.take 1 gameConnectedPlayers)
      return disconnected)
 where
  takeDisconnectedShipId :: StateT ServerState IO (Maybe (Map ShipId Client, ShipId)) -- (connected, disconnected)
  takeDisconnectedShipId =
    get >>= \(ServerState _ _ _ _ _ _ terminate game) ->
      if terminate
        then
          return Nothing
        else
          liftIO (tryReadMVar game) >>= maybe
            (return Nothing)
            (\(CurrentGame _ gamePlayers status) -> case status of
                (Paused _) -> do
                -- we /could/ use 'Paused' ignored argument but it's probably out-of-date
                -- - the game scheduler thread updates it every second only -
                -- so we recompute the difference:
                  connectedPlayers <- onlyPlayersMap
                  let connectedPlayerKeys = Map.keysSet connectedPlayers
                      mayDisconnectedPlayerKey = Set.lookupMin $ Set.difference gamePlayers connectedPlayerKeys
                      gameConnectedPlayers = Map.restrictKeys connectedPlayers gamePlayers
                  return $ maybe
                    Nothing
                    (\disconnectedPlayerKey -> Just (gameConnectedPlayers, disconnectedPlayerKey))
                    mayDisconnectedPlayerKey
                _ -> return Nothing)


addClient :: ShipId -> Connection -> SuggestedPlayerName -> ServerOwnership -> StateT ServerState IO ()
addClient i conn sn cliType = do
  name <- makePlayerName sn
  let client@(Client _ _ _ _ _ _ _ c) = mkClient name i conn cliType
  -- this call is /before/ addClient to avoid sending redundant info to client.
  sendClientsN $ map (RunCommand i) [AssignName name, AssignColors c]
  -- order matters, see comment above.
  addClient' client
  send conn i . ConnectionAccepted i . Map.map
    (\(Client n _ _ _ _ _ _ color) ->
        Player n Present color)
    =<< clientsMap
 where
  addClient' c =
    modify' $ \ s ->
      let clients = getClients s
      in s { getClients =
              clients { getClients' =
                Map.insert i c $ getClients' clients } }

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
  any ((== name) . getName) <$> allClients >>= \case
    True  -> return $ Left "Name is already taken"
    False -> return $ Right ()

shutdown :: Text -> StateT ServerState IO ()
shutdown reason = do
  modify' $ \s -> s { getShouldTerminate = True }
  mapM_ (disconnect $ ServerShutdown reason) =<< allClientsIds

-- It's important that this function doesn't throw any exception.
onBrokenClient :: SomeException -> ShipId -> StateT ServerState IO ()
onBrokenClient e =
  disconnect (BrokenClient $ pack $ show e)

disconnect :: DisconnectReason -> ShipId -> StateT ServerState IO ()
disconnect r i =
  tryRemoveClient >>= maybe
    (return ()) -- the client was not in the client map, we don't do anything.
    (\c@(Client (PlayerName name) _ ownership _ _ _ _ _) -> do
        -- If the client owns the server, we shutdown connections to /other/ clients first.
        when (ownership == ClientOwnsServer) $ do
          clientsMap >>= void . Map.traverseWithKey
            (\j client' -> do
                let msg = "[" <> name <> "] disconnection (hosts the Server) < " <> pack (show r)
                -- Because we pass a 'ServerShutdown', 'disconnectClient' won't
                -- use the clients map so it's safe to just clear the client map
                -- /after/ the entire traversal was done, we won't have infinite recursion.
                disconnectClient (ServerShutdown msg) j client')
          removeAllClients
        -- Finally, shutdown the client connection.
        disconnectClient r i c)
 where
  disconnectClient :: DisconnectReason -> ShipId -> Client -> StateT ServerState IO ()
  disconnectClient reason cid (Client (PlayerName playerName) conn _ _ _ _ _ _) = do
    -- If possible, notify the client about the disconnection
    case reason of
      BrokenClient _ ->
        -- we can't use the client connection anymore.
        -- on its side, the client will probably receive an exception when reading or sending data.
        return ()
      _ -> do
        send conn cid $ Disconnected reason
        liftIO $ sendClose conn $
          "[" <> playerName <> "] disconnection < " <> pack (show reason)
    -- notify other clients about the disconnection of client
    case reason of
      BrokenClient e -> sendClients $ RunCommand cid $ Leaves $ ConnectionError e
      ClientShutdown -> sendClients $ RunCommand cid $ Leaves Intentional
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

allClients :: StateT ServerState IO [Client]
allClients =
  Map.elems . getClients' . getClients <$> get

clientsMap :: StateT ServerState IO (Map ShipId Client)
clientsMap =
  getClients' . getClients <$> get

allClientsIds :: StateT ServerState IO [ShipId]
allClientsIds = Map.keys <$> clientsMap

onlyPlayersMap :: StateT ServerState IO (Map ShipId Client)
onlyPlayersMap = Map.filter (isJust . getState) <$> clientsMap

onlyPlayers :: StateT ServerState IO [Client]
onlyPlayers =
  filter (isJust . getState) <$> allClients

onlyPlayersIds :: StateT ServerState IO (Set ShipId)
onlyPlayersIds =
  Map.keysSet <$> onlyPlayersMap

getPlayersKeys :: StateT ServerState IO (Set ShipId)
getPlayersKeys =
  Map.keysSet <$> onlyPlayersMap

getIntent :: StateT ServerState IO Intent
getIntent = getIntent' <$> get

error' :: Connection -> ShipId -> String -> StateT ServerState IO ()
error' conn i txt = do
  send conn i $ Error $ "*** error from Server: " ++ txt
  error $ "error from Server: " ++ txt -- this error may not be very readable if another thread writes to the console,
    -- hence we sent the error to the client, so that it can report the error too.

gameError :: String -> StateT ServerState IO ()
gameError txt = do
  sendPlayers $ Error $ "*** game error from Server: " ++ txt
  error $ "game error from Server: " ++ txt -- this error may not be very readable if another thread writes to the console,
    -- hence we sent the error to the client, so that it can report the error too.

serverError :: String -> StateT ServerState IO ()
serverError txt = do
  sendClients $ Error $ "*** server error from Server: " ++ txt
  error $ "game error from Server: " ++ txt -- this error may not be very readable if another thread writes to the console,
    -- hence we sent the error to the client, so that it can report the error too.

handleIncomingEvent :: Connection -> ShipId -> ClientEvent -> StateT ServerState IO ()
handleIncomingEvent conn i = \case
  Connect (SuggestedPlayerName sn) _ ->
    errorPriviledged $ "already connected : " <> sn
  RequestCommand cmd@(AssignName name) -> either
    (sendPriviledged . CommandError cmd)
    (\_ -> checkNameAvailability name >>= either
      (sendPriviledged . CommandError cmd)
      (\_ -> do
        modifyClient i $ \c -> c { getName = name }
        sendClients $ RunCommand i cmd)
    ) $ checkName name
  RequestCommand cmd@(AssignColors colors) -> do
    modifyClient i $ \c -> c { getColors = colors }
    sendClients $ RunCommand i cmd
  RequestCommand cmd@(Says _) -> sendClients $ RunCommand i cmd
  RequestCommand (Leaves _) -> disconnect ClientShutdown i -- will do the corresponding 'sendClients $ RunCommand'
  ExitedState Excluded ->
    getIntent >>= \case
      IntentSetup -> do
        sendClients $ PlayerInfo Joins i
        -- change client state to make it playable
        modifyClient i $ \c -> c { getState = Just Finished }
        -- request world to let the client have a world
        requestWorld
        -- next step when client 'IsReady'
      _ -> do
        sendClients $ PlayerInfo WaitsToJoin i
        sendPriviledged $ EnterState Excluded
  ExitedState (PlayLevel _) -> return ()
  LevelEnded outcome -> get >>= \(ServerState _ _ (LevelSpec levelN _) _ _ intent _ game) -> do
    case intent of
      IntentPlayGame ->
        modify' $ \s -> s { getIntent' = IntentLevelEnd outcome }
      IntentLevelEnd o ->
        if o == outcome
          then
            return ()
          else
            gameError $ "inconsistent outcomes:" ++ show (o, outcome)
      IntentSetup ->
        gameError "LevelEnded received while in IntentSetup"
    modifyClient i $ \c -> c { getState = Just Finished }
    onlyPlayers >>= \players -> do
      let playersAllFinished = all (finished . getState) players
          finished Nothing = error "should not happen"
          finished (Just Finished) = True
          finished (Just InGame) = False
      when playersAllFinished $ do
        void $ liftIO $ takeMVar game
        sendClientsN $ (GameInfo $ LevelResult levelN outcome):
                       [GameInfo GameWon | outcome == Won && levelN == lastLevel]
        if outcome == Won && levelN < lastLevel
          then
            modify' $ \s -> let (LevelSpec _ constraint) = getLevelSpec' s
                            in s { getLevelSpec' = LevelSpec (succ levelN) constraint
                                 , getIntent' = IntentPlayGame
                                 }
          else do
            modify' $ \s -> let (LevelSpec _ constraint) = getLevelSpec' s
                            in s { getLevelSpec' = LevelSpec firstServerLevel constraint
                                 , getIntent' = IntentSetup
                                 }
            -- allow fresh clients to become players
            modifyClients' (\c ->
              maybe
                (Just $ c { getState = Just Finished })
                (const Nothing)
                $ getState c) >>= sendClientsN . map (PlayerInfo Joins)
        requestWorld
------------------------------------------------------------------------------
-- Clients in 'Setup' state can configure the world.
------------------------------------------------------------------------------
  ChangeWallDistribution t ->
    onChangeWorldParams $ changeWallDistrib t
  ChangeWorldShape s ->
    onChangeWorldParams $ changeWorldShape s
  ExitedState Setup -> getIntent >>= \case
    IntentSetup -> do
      modify' $ \s -> s { getIntent' = IntentPlayGame }
      sendClients $ PlayerInfo StartsGame i
      sendPlayers $ ExitState Setup
      requestWorld
    _ -> return ()
  WorldProposal essence ->
    get >>= \(ServerState _ _ levelSpec _ lastWid _ _ _) -> do
      let wid = fromMaybe (error "Nothing WorldId in WorldProposal") $ getWorldId essence
      when (lastWid == Just wid) $
        sendPlayers $ ChangeLevel (mkLevelEssence levelSpec) essence
  CurrentGameState gameStateEssence ->
    get >>= \(ServerState _ _ _ _ _ _ _ game) ->
      liftIO (tryReadMVar game) >>= \case
        (Just (CurrentGame _ gamePlayers (Paused _))) -> do
          disconnectedPlayerKeys <- Set.difference gamePlayers <$> onlyPlayersIds
          sendN [PutGameState gameStateEssence] . flip Map.restrictKeys disconnectedPlayerKeys =<< clientsMap
        invalid -> serverError $ "CurrentGameState sent while game is " ++ show invalid
------------------------------------------------------------------------------
-- Clients notify when they have received a given world, and the corresponding
--   UI Animation is over (hence the level shall start, for example.):
------------------------------------------------------------------------------
  IsReady wid -> do
    modifyClient i $ \c -> c { getCurrentWorld = Just wid }
    getIntent >>= \case
      IntentSetup ->
        -- (follow-up from 'ExitedState Excluded')
        -- Allow the client to setup the world, now that the world contains its ship.
        sendPriviledged $ EnterState Setup
      IntentPlayGame ->
        get >>= \(ServerState (Clients clients _) _ _ _ mayLastWId _ _ game) -> maybe
            (return ())
            (\lastWId -> do
              modifyClient i $ \c -> c { getState = Just Finished }
              -- start the game when all players have the right world
              playersKeys <- getPlayersKeys
              let playersAllReady =
                    all ((== Just lastWId) . getCurrentWorld) $ Map.restrictKeys clients playersKeys
              when playersAllReady $ do
                modifyClients $ \c -> if getState c == Just Finished
                                        then c { getState = Just InGame
                                               , getShipAcceleration = zeroCoords }
                                        else c
                liftIO (tryReadMVar game) >>= maybe
                  -- non blocking because all game changes are done inside a modifyMVar
                  -- of 'ServerState' and we are inside one.
                  (void $ liftIO $ putMVar game $ mkCurrentGame lastWId playersKeys)
                  (\(CurrentGame wid' _ _) -> -- reconnection scenario
                      when (wid' /= lastWId) $
                        serverError $ "reconnection failed " ++ show (wid', lastWId)))
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
    sendPlayers $ GameEvent $ LaserShot i dir
  Action Ship dir ->
    modifyClient i $ \c -> c { getShipAcceleration = translateInDir dir $ getShipAcceleration c }
 where
  sendPriviledged = send conn i
  errorPriviledged = error' conn i

gameScheduler :: MVar ServerState -> IO ()
gameScheduler st =
  readMVar st >>= \(ServerState _ _ _ _ _ _ terminate mayGame) ->
    if terminate
      then return ()
      else
        -- block until 'getScheduledGame' contains a 'CurrentGame'
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
    modifyClients $ \c -> c { getShipSafeUntil = Just $ addDuration (fromSecs 6) start }

  stepWorld :: Time Point System -> StateT ServerState IO ()
  stepWorld now = do
    let !zero = zeroCoords
    accs <- Map.mapMaybe
      (\p ->
          let !acc = getShipAcceleration p
            in if acc == zero
              then Nothing
              else Just acc)
      <$> onlyPlayersMap
    becameSafe <- updateSafeShips
    sendPlayers $ GameEvent $ PeriodicMotion accs becameSafe
    modifyClients $ \p -> p { getShipAcceleration = zeroCoords }
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
  run' refWid act = get >>= \(ServerState _ _ _ _ _ _ terminate mayWorld) ->
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
                missingPlayers <- Set.difference gamePlayers <$> getPlayersKeys
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
                    sendPlayers $ EnterState $ PlayLevel newStatus
                    return NotExecutedTryAgainLater
                  else
                    case newStatus of
                      New -> do
                        gameError "logic : newStatus == New"
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

modifyClient :: ShipId -> (Client -> Client) -> StateT ServerState IO ()
modifyClient i f =
  modify' $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.adjust f i $ getClients' clients
                    }
          }

modifyClients :: (Client -> Client) -> StateT ServerState IO ()
modifyClients f =
  modify' $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.map f $ getClients' clients
                    }
          }

modifyClients' :: (Client -> Maybe Client) -> StateT ServerState IO [ShipId]
modifyClients' f =
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
  get >>= \(ServerState _ _ level params wid _ _ _) -> do
    shipIds <- onlyPlayersIds
    sendFirstWorldBuilder $ WorldRequest $ WorldSpec level shipIds params wid
 where
  incrementWorldId =
    modify' $ \s ->
      let wid = maybe (WorldId 0) succ $ getLastRequestedWorldId' s
      in s { getLastRequestedWorldId' = Just wid }


onChangeWorldParams :: (WorldParameters -> WorldParameters)
                    -> StateT ServerState IO ()
onChangeWorldParams f = do
  modify' $ \s ->
    s { getWorldParameters = f $ getWorldParameters s }
  requestWorld

changeWallDistrib :: WallDistribution -> WorldParameters -> WorldParameters
changeWallDistrib d p = p { getWallDistrib = d }

changeWorldShape :: WorldShape -> WorldParameters -> WorldParameters
changeWorldShape d p = p { getWorldShape = d }

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
