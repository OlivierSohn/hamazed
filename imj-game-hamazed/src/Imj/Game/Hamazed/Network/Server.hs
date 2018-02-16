{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import           Imj.Prelude hiding (drop, intercalate)

import           Control.Concurrent(threadDelay)
import           Control.Concurrent.MVar (MVar
                                        , modifyMVar_, modifyMVar
                                        , readMVar, tryReadMVar, putMVar, takeMVar) -- for signaling purposes
import           Control.Monad (forever)
import           Control.Monad.State.Strict(StateT, runStateT, execStateT, modify', get, state)
import           Data.Char (isPunctuation, isSpace, toLower)
import qualified Data.Map.Strict as Map(map, elems, adjust, insert, delete, member, mapAccum)
import           Data.Maybe(isJust)
import           Data.Text(pack)
import           Data.Tuple(swap)
import           Network.WebSockets
                  (PendingConnection, WebSocketsData(..), Connection, DataMessage(..),
                  -- the functions on the mext line throw(IO), hence we catch exceptions
                  -- to remove the client from the map when needed.
                   acceptRequest, sendBinaryData, sendDataMessage, sendClose, receiveData,
                   forkPingThread)
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

mkServer :: (Maybe ServerName) -> ServerPort -> Server
mkServer Nothing = Local
mkServer (Just (ServerName n)) =
  Distant $ ServerName $ map toLower n

removeClient :: ShipId -> Clients -> Clients
removeClient i clients =
  clients { getClients' = Map.delete i $ getClients' clients }

send :: Client -> ServerEvent -> StateT ServerState IO ()
send client evt =
  sendNBinaryData evt [client]

sendClients :: ServerEvent -> StateT ServerState IO ()
sendClients evt =
  sendNBinaryData evt =<< allClients

sendPlayers :: ServerEvent -> StateT ServerState IO ()
sendPlayers evt =
  sendNBinaryData evt =<< onlyPlayers

sendFirstWorldBuilder :: ServerEvent -> StateT ServerState IO ()
sendFirstWorldBuilder evt =
  sendNBinaryData evt . take 1 =<< allClients

{-# INLINABLE sendNBinaryData #-}
-- | Uses sendDataMessage which is at a lower-level than sendBinaryData
-- to factorize serialization.
--
-- In case some connections are closed, the corresponding clients are silently removed
-- from the Map, and we continue the processing.
sendNBinaryData :: WebSocketsData a
                => a
                -> [Client]
                -> StateT ServerState IO ()
sendNBinaryData a clients =
  let !msg = Binary $ toLazyByteString a
  in mapM_ (sendAndHandleExceptions msg) clients
 where
  sendAndHandleExceptions m x = do
    liftIO (try (sendDataMessage (getConnection x) m)) >>= either
      (\(e :: SomeException) -> onBrokenClient e x)
      return

appSrv :: MVar ServerState -> PendingConnection -> IO ()
appSrv st pending =
  acceptRequest pending >>= appSrv' st

appSrv' :: MVar ServerState -> Connection -> IO ()
appSrv' st conn = do
  forkPingThread conn 30 -- to keep the connection alive (TODO should we use keepalive socket property instead?)
  receiveData conn >>= \case
    Connect sn@(SuggestedPlayerName suggestedName) cliType ->
      if any ($ suggestedName) [ null, any isPunctuation, any isSpace]
        then
          sendBinaryData conn $ ConnectionRefused $ InvalidName sn $
            "Name cannot contain punctuation or whitespace, and cannot be empty"
        else do
          client <- modifyMVar st (fmap swap . runStateT (makeClient conn sn cliType))
          try
            (do
              sendBinaryData conn $ ConnectionAccepted $ getIdentity client
              forever $
                receiveData conn >>= modifyMVar_ st . execStateT . handleIncomingEvent client) >>= either
            (\(e :: SomeException) -> modifyMVar_ st $ execStateT $ onBrokenClient e client)
            return
    msg -> error $ "first received msg is not Connect : " ++ show msg


makeClient :: Connection -> SuggestedPlayerName -> ClientType -> StateT ServerState IO Client
makeClient conn sn cliType = do
  cId <- ClientId <$> makePlayerName sn <*> takeShipId
  let client = mkClient cId conn cliType
  addClient client
  return client
 where
  takeShipId =
    state $ \s ->
      let newShipId = succ $ getNextShipId $ getClients s
      in ( newShipId, s { getClients = (getClients s) { getNextShipId = newShipId } } )
  addClient c =
    modify' $ \ s ->
      let clients = getClients s
      in s { getClients =
              clients { getClients' =
                Map.insert (getClientId $ getIdentity c) c
                  $ getClients' clients } }

makePlayerName :: SuggestedPlayerName -> StateT ServerState IO PlayerName
makePlayerName (SuggestedPlayerName sn) = do
  let go mayI = do
        let proposal = PlayerName $ pack $ maybe sn ((++) sn . show) mayI
        playerNameIsAlreadyTaken proposal >>= \case
          True -> go $ Just $ maybe (2::Int) succ mayI
          False -> return proposal
  go Nothing
 where
  playerNameIsAlreadyTaken name =
    return . any ((== name) . getPlayerName . getIdentity ) =<< allClients

shutdown :: Text -> StateT ServerState IO ()
shutdown reason = do
  modify' $ \s -> s { getShouldTerminate = True }
  allClients >>= mapM_ (disconnect $ ServerShutdown reason)

-- It's important that this function doesn't throw any exception.
onBrokenClient :: SomeException -> Client -> StateT ServerState IO ()
onBrokenClient e =
  disconnect (BrokenClient $ pack $ show e)

disconnect :: DisconnectReason -> Client -> StateT ServerState IO ()
disconnect r c@(Client i@(ClientId (PlayerName name) _) _ (ClientType ownership) _ _ _ _) =
  get >>= \s -> do
    let clients = getClients' $ getClients s
    -- If the client is not in the client map, we don't do anything.
    when (Map.member (getClientId i) clients) $ do
      -- If the client owns the server, we shutdown connections to /other/ clients first.
      when (ownership == ClientOwnsServer) $
        mapM_
          (\client'@(Client j _ _ _ _ _ _) ->
              unless (i == j) $
                let msg = "[" <> name <> "] disconnection (hosts the Server) << " <> pack (show r)
                in disconnectClient (ServerShutdown msg) client')
          clients
      -- Finally, shutdown the client connection.
      disconnectClient r c
 where
  disconnectClient :: DisconnectReason -> Client -> StateT ServerState IO ()
  disconnectClient reason client@(Client cId@(ClientId (PlayerName name) x) conn _ _ _ _ _) = do
    -- Remove the client from the registered clients Map
    modify' $ \s -> s { getClients = removeClient x $ getClients s }
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
        msg = "[" <> name <> "] disconnection << " <> pack (show reason)

    -- notify other clients about the disconnection of client
    case reason of
      ServerShutdown _ ->
        -- no need to notify other clients, as they will be diconnected too, thus
        -- they will receive a server shutdown notification.
        return ()
      ClientShutdown ->
        sendClients $ PlayerInfo cId $ Leaves Intentional
      BrokenClient e ->
        sendClients $ PlayerInfo cId $ Leaves $ ConnectionError e

allClients :: StateT ServerState IO [Client]
allClients =
  Map.elems . getClients' . getClients <$> get

onlyPlayers :: StateT ServerState IO [Client]
onlyPlayers =
  filter (isJust . getState) <$> allClients

getIntent :: StateT ServerState IO Intent
getIntent = getIntent' <$> get

getLastRequestedWorldId :: StateT ServerState IO (Maybe WorldId)
getLastRequestedWorldId = getLastRequestedWorldId' <$> get

error' :: Client -> String -> StateT ServerState IO ()
error' client txt = do
  send client $ Error $ "*** error from Server: " ++ txt
  error $ "error from Server: " ++ txt -- this error may not be very readable if another thread writes to the console,
    -- hence we sent the error to the client, so that it can report the error too.

handleIncomingEvent :: Client -> ClientEvent -> StateT ServerState IO ()
handleIncomingEvent client@(Client cId _ _ _ _ _ _) = \case
  Connect (SuggestedPlayerName sn) _ ->
    error' client $ "already connected : " <> sn
  Disconnect ->
    disconnect ClientShutdown client
  Say what ->
    sendClients $ PlayerInfo cId $ Says what
  -- TODO are "EnteredState" events useful?
  EnteredState Excluded -> return ()
  EnteredState PlayLevel -> return ()
  ExitedState Excluded -> do
    send client . ListPlayers . map (getPlayerName . getIdentity) =<< allClients
    sendClients $ PlayerInfo cId Joins
    getIntent >>= \case
      Intent'Setup -> do
        -- change client state to make it playable
        modifyClient cId $ \c -> c { getState = Just Finished }
        -- request world to let the client have a world
        requestWorld
        -- next step when client 'IsReady'
      _ -> send client $ EnterState Excluded
  ExitedState PlayLevel -> return ()
  LevelEnded outcome -> do
    getIntent >>= \case
      Intent'PlayGame ->
        modify' $ \s -> s { getIntent' = Intent'LevelEnd outcome }
      Intent'LevelEnd o ->
        if o == outcome
          then
            return ()
          else
            error $ "inconsistent outcomes:" ++ show (o, outcome)
      Intent'Setup -> error "logic"
    modifyClient cId $ \c -> c { getState = Just Finished }
    send client $ ExitState PlayLevel
    onlyPlayers >>= \players -> do
      let playersAllFinished = all (finished . getState) players
          finished Nothing = error "should not happen"
          finished (Just Finished) = True
          finished (Just InGame) = False
      when playersAllFinished $ do
        void $ getSchedulerSignal <$> get >>= liftIO . takeMVar
        n <- getLevelNumber' . getLevelSpec <$> get
        sendClients $ GameInfo $ LevelResult n outcome
        when (outcome == Won && n == lastLevel) $
          sendClients $ GameInfo GameWon
        if outcome == Won && n < lastLevel
           then
             modify' $ \s -> s { getLevelSpec = mkLevelSpec $ succ n
                              , getIntent' = Intent'PlayGame
                              }
           else do
             modify' $ \s -> s { getLevelSpec = mkLevelSpec firstLevel
                              , getIntent' = Intent'Setup
                              }
             modifyClients $ \c -> c { getState = Just Finished } -- so that fresh clients become players
        requestWorld
------------------------------------------------------------------------------
-- Clients in 'Setup' state can configure the world.
--
--   [If /any/ client is in 'Setup' state, /no/ client is in 'PlayLevel' state]
------------------------------------------------------------------------------
  EnteredState Setup -> return ()
  ChangeWallDistribution t ->
    onChangeWorldParams $ changeWallDistrib t
  ChangeWorldShape s ->
    onChangeWorldParams $ changeWorldShape s
  ExitedState Setup ->
    getIntent >>= \case
      Intent'Setup -> do
        modify' $ \s -> s { getIntent' = Intent'PlayGame }
        sendClients $ PlayerInfo cId StartsGame
        sendPlayers $ ExitState Setup -- prevent other playing clients from modifying the world parameters
        requestWorld
      _ -> return ()
------------------------------------------------------------------------------
-- A 'WorldCreator' client may be requested to propose a world:
------------------------------------------------------------------------------
  WorldProposal essence ->
    get >>= \s -> do
      let wid = fromMaybe (error "Nothing WorldId in WorldProposal") $ getWorldId essence
      when (getLastRequestedWorldId' s == Just wid) $ do
        sendPlayers $ ChangeLevel (getLevelSpec s) essence
------------------------------------------------------------------------------
-- Clients notify when they have received a given world, and the corresponding
--   UI Animation is over (hence the level shall start, for example.):
------------------------------------------------------------------------------
  IsReady wid -> do
    modifyClient cId $ \c -> c { getCurrentWorld = Just wid }
    getIntent >>= \case
      Intent'Setup ->
        -- (follow-up from 'ExitedState Excluded')
        -- Allow the client to setup the world, now that the world contains its ship.
        send client $ EnterState Setup
      Intent'PlayGame ->
          getLastRequestedWorldId >>= maybe
            (return ())
            (\lastWId -> do
              -- start the game when all players have the right world
              playersAllReady <-
                all ((== Just lastWId) . getCurrentWorld)
                  . filter ((== Just Finished) . getState)
                  <$> allClients
              when playersAllReady $ do
                modifyClients $ \c -> if getState c == Just Finished
                                        then c { getState = Just InGame
                                               , getShipAcceleration = zeroCoords }
                                        else c
                sendPlayers $ EnterState PlayLevel
                getSchedulerSignal <$> get >>= liftIO . flip putMVar lastWId)
      Intent'LevelEnd _ -> return ()
------------------------------------------------------------------------------
-- Clients in 'PlayLevel' state can play the game.
--
--   [If /any/ client is in 'PlayLevel' state, /no/ client is in 'Setup' state]
------------------------------------------------------------------------------
  -- Due to network latency, laser shots may be applied to a world state
  -- different from what the player saw when the shot was made.
  -- But since the laser shot will be rendered with latency, too, the player will be
  -- able to integrate the latency via this visual feedback - I read somewhere that
  -- latency is not a big problem for players, provided that it is stable over time.
  Action Laser dir ->
    sendPlayers $ GameEvent $ LaserShot (getClientId cId) dir
  Action Ship dir ->
    modifyClient cId $ \c -> c { getShipAcceleration = translateInDir dir $ getShipAcceleration c }

gameScheduler :: MVar ServerState -> IO ()
gameScheduler st =
  readMVar st >>= \(ServerState _ _ _ _ _ _ terminate mayWorld) ->
    if terminate
      then return ()
      else
        -- block until 'getSchedulerSignal' contains a 'WorldId'
        readMVar mayWorld >>= \currentWorld -> do
          startTime <- getSystemTime
          ok <- modifyMVar st (fmap swap . runStateT ((whenCanContinue currentWorld) (initializePlayers startTime)))
          let mult = initalGameMultiplicator
              go mayPrevUpdate = do
                now <- getSystemTime
                let baseTime = fromMaybe now mayPrevUpdate
                    update = addDuration (toSystemDuration mult gameMotionPeriod) baseTime
                threadDelay $ fromIntegral $ toMicros $ now...update
                goOn <- modifyMVar st (fmap swap . runStateT ((whenCanContinue currentWorld) (stepWorld now)))
                when goOn $ go $ Just update
          when ok $ go Nothing
          gameScheduler st
 where
  initializePlayers :: Time Point System -> StateT ServerState IO ()
  initializePlayers start =
    modifyClients $ \c -> c { getShipSafeUntil = Just $ addDuration (fromSecs 5) start }
  stepWorld :: Time Point System -> StateT ServerState IO ()
  stepWorld now = do
    let !zero = zeroCoords
    accs <- mapMaybe
      (\p -> let !acc = getShipAcceleration p
             in if acc == zero
                  then Nothing
                  else Just (getClientId $ getIdentity p, acc))
      <$> onlyPlayers
    becameSafe <- updateSafeShips
    sendPlayers $ GameEvent $ PeriodicMotion accs $ map (getClientId . getIdentity) becameSafe
    modifyClients $ \p -> p { getShipAcceleration = zeroCoords }
   where
    updateSafeShips =
      state $ \s ->
        let clients = getClients s
            (clientsMadeSafe, c') = Map.mapAccum (\shipsMadeSafe client ->
                              maybe
                                (shipsMadeSafe, client)
                                (\shipUnsafeAt ->
                                    if shipUnsafeAt < now
                                      then (client:shipsMadeSafe, client { getShipSafeUntil = Nothing } )
                                      else (shipsMadeSafe, client))
                                $ getShipSafeUntil client) [] (getClients' clients)
        -- mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
        in (clientsMadeSafe, s { getClients = clients { getClients' = c' } })

  whenCanContinue :: WorldId -> StateT ServerState IO () -> StateT ServerState IO Bool
  whenCanContinue wid act = get >>= \(ServerState _ _ _ _ _ _ terminate mayWorld) ->
    if terminate
      then return False
      else
        -- we use 'tryReadMVar' to /not/ block here, as we are inside a modifyMVar.
        liftIO (tryReadMVar mayWorld) >>= maybe
          (return False)
          (\curWid -> do
            if wid /= curWid
              then
                return False -- the world has changed
              else do
                act
                return True)

modifyClient :: ClientId -> (Client -> Client) -> StateT ServerState IO ()
modifyClient cId f =
  modify' $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.adjust f (getClientId cId) $ getClients' clients
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

requestWorld :: StateT ServerState IO ()
requestWorld = do
  incrementWorldId
  get >>= \(ServerState _ _ (LevelSpec level _ _) params wid _ _ _) -> do
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
-- TODOs:
-- ship safe : on level start : addDuration (fromSecs 5) t

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
