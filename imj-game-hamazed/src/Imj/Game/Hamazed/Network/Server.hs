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
import           Data.Map.Strict(Map, (!?))
import qualified Data.Map.Strict as Map(map, mapMaybe, union, elems, keys, adjust, insert, delete, member, mapAccum
                                      , foldMapWithKey, restrictKeys)
import           Data.Set (Set)
import qualified Data.Set as Set (difference, intersection, fromAscList, lookupMin)
import           Data.Maybe(isJust, maybeToList)
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

removeClient :: ShipId -> Clients -> Clients
removeClient i clients =
  clients { getClients' = Map.delete i $ getClients' clients }

send :: Client -> ServerEvent -> StateT ServerState IO ()
send client evt =
  sendN [evt] [client]

sendClients :: ServerEvent -> StateT ServerState IO ()
sendClients evt =
  sendN [evt] =<< allClients

sendClientsN :: [ServerEvent] -> StateT ServerState IO ()
sendClientsN evts =
  sendN evts =<< allClients

sendPlayers :: ServerEvent -> StateT ServerState IO ()
sendPlayers evt =
  sendN [evt] =<< onlyPlayers

sendFirstWorldBuilder :: ServerEvent -> StateT ServerState IO ()
sendFirstWorldBuilder evt =
  sendN [evt] . take 1 =<< allClients

{-# INLINABLE sendN #-}
-- | Uses sendDataMessage which is at a lower-level than sendBinaryData
-- to factorize serialization.
--
-- In case some connections are closed, the corresponding clients are silently removed
-- from the Map, and we continue the processing.
sendN :: WebSocketsData a
                => [a]
                -> [Client]
                -> StateT ServerState IO ()
sendN a clients =
  let !msgs = map (Binary . toLazyByteString) a
  in mapM_ (sendAndHandleExceptions msgs) clients
 where
  sendAndHandleExceptions m x =
    liftIO (try (sendDataMessages (getConnection x) m)) >>= either
      (\(e :: SomeException) -> onBrokenClient e x)
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
        client <- modifyMVar st (fmap swap . runStateT (makeClient conn sn cliType))
        let disconnectOnException action = try action >>= either
              (\(e :: SomeException) -> modifyMVar_ st $ execStateT $ onBrokenClient e client)
              return
        -- To detect disconnections when communication is idle:
        void $ forkIO $ disconnectOnException $ pingPong conn $ fromSecs 1
        disconnectOnException $
          forever $ receiveData conn >>= modifyMVar_ st . execStateT . handleIncomingEvent client
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

makeClient :: Connection -> SuggestedPlayerName -> ServerOwnership -> StateT ServerState IO Client
makeClient conn sn cliType = do
  i <- takeShipId
  name <- makePlayerName sn
  let client@(Client _ _ _ _ _ _ _ _ c) = mkClient name i conn cliType
  -- this call is /before/ addClient to avoid sending redundant info to client.
  sendClientsN $ map (RunCommand i) [AssignName name, AssignColors c]
  -- order matters, see comment above.
  addClient client
  send client . ConnectionAccepted i . Map.map
    (\(Client _ n  _ _ _ _ _ _ color) ->
        Player n Present color)
    =<< clientsMap
  return client
 where
  takeDisconnectedShipId :: StateT ServerState IO (Maybe (Maybe Client, ShipId)) -- (connected, disconnected)
  takeDisconnectedShipId =
    get >>= \(ServerState (Clients clients _) _ _ _ _ _ terminate game) ->
      if terminate
        then
          return Nothing
        else
          maybe
            Nothing
            (\(CurrentGame _ gamePlayers status) -> case status of
                (Paused _) ->
                -- we /could/ use 'Paused' ignored argument but it's probably out-of-date
                -- - the game scheduler thread updates it every second only -
                -- so we recompute the difference:
                  let connectedPlayerKeys = getPlayersKeys clients
                      mayDisconnectedPlayerKey = Set.lookupMin $ Set.difference gamePlayers connectedPlayerKeys
                      -- gamePlayers /should be/ included in connectedPlayerKeys, but just to be sure,
                      -- we do the intersection.
                      mayConnectedPlayerKey = Set.lookupMin $ Set.intersection gamePlayers connectedPlayerKeys
                      mayConnectedPlayer = join $ fmap (clients !?) mayConnectedPlayerKey
                  in maybe
                        Nothing
                        (\disconnectedPlayerKey -> Just (mayConnectedPlayer, disconnectedPlayerKey))
                        mayDisconnectedPlayerKey
                _ -> Nothing)
            <$> liftIO (tryReadMVar game)
  takeShipId =
    takeDisconnectedShipId >>= maybe
      (state $ \s ->
        let newShipId = succ $ getNextShipId $ getClients s
        in ( newShipId, s { getClients = (getClients s) { getNextShipId = newShipId } } ))
      (\(mayConnected, disconnected) -> do
        maybe
          (serverError "the current game has no connected player") -- TODO support that
          (`send` CurrentGameStateRequest)
          mayConnected
        return disconnected)
  addClient c =
    modify' $ \ s ->
      let clients = getClients s
      in s { getClients =
              clients { getClients' =
                Map.insert (getIdentity c) c
                  $ getClients' clients } }

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
  mapM_ (disconnect $ ServerShutdown reason) =<< allClients

-- It's important that this function doesn't throw any exception.
onBrokenClient :: SomeException -> Client -> StateT ServerState IO ()
onBrokenClient e =
  disconnect (BrokenClient $ pack $ show e)

disconnect :: DisconnectReason -> Client -> StateT ServerState IO ()
disconnect r c@(Client i (PlayerName name) _ ownership _ _ _ _ _) =
  get >>= \s -> do
    let clients = getClients' $ getClients s
    -- If the client is not in the client map, we don't do anything.
    when (Map.member i clients) $ do
      -- If the client owns the server, we shutdown connections to /other/ clients first.
      when (ownership == ClientOwnsServer) $
        mapM_
          (\client'@(Client j _ _ _ _ _ _ _ _) ->
              unless (i == j) $ do
                let msg = "[" <> name <> "] disconnection (hosts the Server) < " <> pack (show r)
                disconnectClient (ServerShutdown msg) client')
          clients
      -- Finally, shutdown the client connection.
      disconnectClient r c
 where
  disconnectClient :: DisconnectReason -> Client -> StateT ServerState IO ()
  disconnectClient reason client@(Client cid (PlayerName playerName) conn _ _ _ _ _ _) = do
    -- Remove the client from the registered clients Map
    modify' $ \s -> s { getClients = removeClient cid $ getClients s }
    -- If possible, notify the client about the disconnection
    case reason of
      BrokenClient _ ->
        -- we can't use the client connection anymore.
        -- on its side, the client will probably receive an exception when reading or sending data.
        return ()
      _ -> do
        send client $ Disconnected reason
        liftIO $ sendClose conn msg
       where
        msg = "[" <> playerName <> "] disconnection < " <> pack (show reason)

    -- notify other clients about the disconnection of client
    case reason of
      BrokenClient e -> sendClients $ RunCommand cid $ Leaves $ ConnectionError e
      ClientShutdown -> sendClients $ RunCommand cid $ Leaves Intentional
      ServerShutdown _ -> return () -- no need to notify other clients, as they will be diconnected too,
                                    -- hence they will receive a server shutdown notification.

allClients :: StateT ServerState IO [Client]
allClients =
  Map.elems . getClients' . getClients <$> get

clientsMap :: StateT ServerState IO (Map ShipId Client)
clientsMap =
  getClients' . getClients <$> get

onlyPlayers :: StateT ServerState IO [Client]
onlyPlayers =
  filter (isJust . getState) <$> allClients

getIntent :: StateT ServerState IO Intent
getIntent = getIntent' <$> get

error' :: Client -> String -> StateT ServerState IO ()
error' client txt = do
  send client $ Error $ "*** error from Server: " ++ txt
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

handleIncomingEvent :: Client -> ClientEvent -> StateT ServerState IO ()
handleIncomingEvent client@(Client i _ _ _ _ _ _ _ _) = \case
  Connect (SuggestedPlayerName sn) _ ->
    error' client $ "already connected : " <> sn
  RequestCommand cmd@(AssignName name) -> either
    (send client . CommandError cmd)
    (\_ -> checkNameAvailability name >>= either
      (send client . CommandError cmd)
      (\_ -> do
        modifyClient i $ \c -> c { getName = name }
        sendClients $ RunCommand i cmd)
    ) $ checkName name
  RequestCommand cmd@(AssignColors colors) -> do
    modifyClient i $ \c -> c { getColors = colors }
    sendClients $ RunCommand i cmd
  RequestCommand cmd@(Says _) -> sendClients $ RunCommand i cmd
  RequestCommand (Leaves _) -> disconnect ClientShutdown client -- will do the corresponding 'sendClients $ RunCommand'
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
        send client $ EnterState Excluded
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
    get >>= \(ServerState (Clients clients _) _ _ _ _ _ _ game) ->
      liftIO (tryReadMVar game) >>= \case
        (Just (CurrentGame _ gamePlayers (Paused _))) -> do
          let disconnectedPlayerKeys = Set.difference gamePlayers $ getPlayersKeys clients
              disconnectedClients = Map.elems $ Map.restrictKeys clients disconnectedPlayerKeys
          sendN [PutGameState gameStateEssence] disconnectedClients
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
        send client $ EnterState Setup
      IntentPlayGame ->
        get >>= \(ServerState (Clients clients _) _ _ _ mayLastWId _ _ game) -> maybe
            (return ())
            (\lastWId -> do
              modifyClient i $ \c -> c { getState = Just Finished }
              -- start the game when all players have the right world
              let playersKeys = getPlayersKeys clients
                  playersAllReady =
                    all ((== Just lastWId) . getCurrentWorld) $
                      Map.restrictKeys clients playersKeys
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
    accs <- mapMaybe
      (\p -> let !acc = getShipAcceleration p
             in if acc == zero
                  then Nothing
                  else Just (getIdentity p, acc))
      <$> onlyPlayers
    becameSafe <- updateSafeShips
    sendPlayers $ GameEvent $ PeriodicMotion accs $ map getIdentity becameSafe
    modifyClients $ \p -> p { getShipAcceleration = zeroCoords }
   where
    updateSafeShips =
      state $ \s ->
        let clients = getClients s
            (clientsMadeSafe, c') =
              Map.mapAccum
                (\shipsMadeSafe client -> maybe
                  (shipsMadeSafe, client)
                  (\shipUnsafeAt ->
                      if shipUnsafeAt < now
                        then
                          (client:shipsMadeSafe, client { getShipSafeUntil = Nothing } )
                        else
                          (shipsMadeSafe, client))
                  $ getShipSafeUntil client)
                [] $ getClients' clients
        in (clientsMadeSafe, s { getClients = clients { getClients' = c' } })

  -- run the action if the world matches the current world and the game was
  -- not terminated
  run' :: WorldId -> StateT ServerState IO () -> StateT ServerState IO RunResult
  run' refWid act = get >>= \(ServerState (Clients clients _) _ _ _ _ _ terminate mayWorld) ->
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
                let missingPlayers = Set.difference gamePlayers $ getPlayersKeys clients
                    newStatus =
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

getPlayersKeys :: Map ShipId Client -> Set ShipId
getPlayersKeys =
  Set.fromAscList . Map.foldMapWithKey
    (\cid client -> maybeToList $
        if isJust $ getState client
          then
            Just cid
          else
            Nothing)

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
    shipIds <- map getIdentity <$> onlyPlayers
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
       => [(ShipId, Coords Vel)]
       -> [ShipId]
       -> m ()
onMove accelerations shipsLosingArmor = getGameState >>= \(GameState _ m@(Multiplicator mv) world a b c d e) ->
  liftIO getSystemTime >>= \t -> do
    let nextTime = addDuration (toSystemDuration m gameMotionPeriod) t
    putGameState $ GameState (Just nextTime) (Multiplicator (mv + 0.01)) (moveWorld accelerations shipsLosingArmor world) a b c d e
    onHasMoved t
-}
