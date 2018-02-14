{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Network.Server
      ( ClientNode(..)
      , Server
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
import           Control.Monad.State.Strict(StateT, runStateT, execStateT, StateT, modify, get, state)
import           Data.Char (isPunctuation, isSpace, toLower)
import qualified Data.Map.Strict as Map(map, elems, adjust, insert, delete, member)
import           Data.Maybe(isJust)
import           Data.Text(pack)
import           Data.Tuple(swap)
import           Network.WebSockets
                  (PendingConnection, sendBinaryData, receiveData, acceptRequest, forkPingThread
                  , WebSocketsData(..), Connection, DataMessage(..), sendDataMessage, sendClose)
import           UnliftIO.Exception (finally)

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

{-# INLINABLE sendNBinaryData #-}
sendNBinaryData :: WebSocketsData a => [Connection] -> a -> IO ()
sendNBinaryData conns a =
  let serialized = Binary $ toLazyByteString a
  in mapM_ (flip sendDataMessage serialized) conns

send :: Client -> ServerEvent -> StateT ServerState IO ()
send client evt = liftIO $ sendBinaryData (getConnection client) evt

sendClients' :: ServerEvent -> [Client] -> IO ()
sendClients' evt = flip sendNBinaryData evt . map getConnection

sendClients :: ServerEvent -> StateT ServerState IO ()
sendClients evt =
  liftIO . sendClients' evt =<< allClients

sendPlayers :: ServerEvent -> StateT ServerState IO ()
sendPlayers evt =
  liftIO . sendClients' evt =<< onlyPlayers

appSrv :: MVar ServerState -> PendingConnection -> IO ()
appSrv st pending =
  acceptRequest pending >>= appSrv' st

appSrv' :: MVar ServerState -> Connection -> IO ()
appSrv' st conn = do
  forkPingThread conn 30 -- to keep the connection alive (TODO should we use keepalive socket property instead?)
  msg <- receiveData conn
  case msg of
    Connect sn@(SuggestedPlayerName suggestedName) cliType ->
      if any ($ suggestedName) [ null, any isPunctuation, any isSpace]
        then
          sendBinaryData conn $ ConnectionRefused $ InvalidName sn $
            "Name cannot contain punctuation or whitespace, and cannot be empty"
        else do
          client <- modifyMVar st (fmap swap . runStateT (makeClient conn sn cliType))
          flip finally
            (modifyMVar_ st $ execStateT $ disconnect ByServer client) $ do
              sendBinaryData conn $ ConnectionAccepted $ getIdentity client
              forever $
                receiveData conn >>= modifyMVar_ st . execStateT . handleIncomingEvent client
    _ -> error $ "first received msg is not Connect : " ++ show msg


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
    modify $ \ s ->
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

disconnect :: By -> Client -> StateT ServerState IO ()
disconnect initiatedBy client@(Client i _ (ClientType _ ownership) _ _ _ _) = do
  get >>= \s -> do
    let clients = getClients' $ getClients s
    -- If the client is not in the client map, we don't do anything.
    when (Map.member (getClientId i) clients) $ do
      -- If the client initiated its own disconnection, we broadcast the
      -- information.
      case initiatedBy of
        ByClient initiator ->
          when (initiator == i) $ sendClients $ PlayerInfo i Leaves
        _ -> return ()
      -- If the client owns the server, we shutdown connections to /other/ clients first.
      when (ownership == ClientOwnsServer) $
        mapM_
          (\client'@(Client j _ _ _ _ _ _) ->
              unless (i == j) $ disconnectAtomic ByServer client')
          clients
      -- Shutdown the client connection.
      disconnectAtomic initiatedBy client

disconnectAtomic :: By -> Client -> StateT ServerState IO ()
disconnectAtomic by client@(Client (ClientId (PlayerName name) i) conn _ _ _ _ _) = do
  modify $ \s -> s { getClients = removeClient i $ getClients s }
  send client $ Disconnected by
  liftIO $ sendClose conn msg
 where
  msg = "Graceful disconnection of '" <> name <> "', initiated by " <> showInitiator by <> "."
  showInitiator ByServer = "Server"
  showInitiator (ByClient (ClientId (PlayerName n) _)) = "'" <> n <> "'"


allClients :: StateT ServerState IO [Client]
allClients = Map.elems . getClients' . getClients <$> get

onlyPlayers :: StateT ServerState IO [Client]
onlyPlayers = onlyPlayers' <$> allClients

onlyPlayers' :: [Client] -> [Client]
onlyPlayers' = filter (isJust . getState)

getIntent :: StateT ServerState IO Intent
getIntent = getIntent' <$> get

getLastRequestedWorldId :: StateT ServerState IO (Maybe WorldId)
getLastRequestedWorldId = getLastRequestedWorldId' <$> get

error' :: Client -> String -> StateT ServerState IO ()
error' client txt = do
  send client $ Error $ "*** error from Server: " ++ txt
  error $ "error in Server: " ++ txt -- this error may not be very readable if another thread writes to the console,
    -- hence we sent the error to the client, so that it can error too.

handleIncomingEvent :: Client -> ClientEvent -> StateT ServerState IO ()
handleIncomingEvent client@(Client cId _ _ _ _ _ _) = \case
  Connect (SuggestedPlayerName sn) _ ->
     error' client $ "already connected : " <> sn
  Disconnect ->
    disconnect (ByClient cId) client
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
        modify $ \s -> s { getIntent' = Intent'LevelEnd outcome }
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
             modify $ \s -> s { getLevelSpec = mkLevelSpec $ succ n
                              , getIntent' = Intent'PlayGame
                              }
           else do
             modify $ \s -> s { getLevelSpec = mkLevelSpec firstLevel
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
        modify $ \s -> s { getIntent' = Intent'PlayGame }
        sendClients $ PlayerInfo cId StartsGame
        sendPlayers $ ExitState Setup -- prevent other playing clients from modifying the world parameters
        requestWorld
      _ -> return ()
------------------------------------------------------------------------------
-- A 'WorldCreator' client may be requested to propose a world:
------------------------------------------------------------------------------
  (WorldProposal essence) ->
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
  Action Laser dir -> sendPlayers $ GameEvent $ LaserShot (getClientId cId) dir
  Action Ship dir ->
    modifyClient cId $ \c -> c { getShipAcceleration = translateInDir dir $ getShipAcceleration c }

gameScheduler :: MVar ServerState -> IO ()
gameScheduler st =
  void $ forever $ do
    currentWorld <- fmap getSchedulerSignal (readMVar st) >>= readMVar
    let mult = initalGameMultiplicator
        go mayPrevUpdate = do
          now <- getSystemTime
          let baseTime = fromMaybe now mayPrevUpdate
              update = addDuration (toSystemDuration mult gameMotionPeriod) baseTime
          threadDelay $ fromIntegral $ toMicros $ now...update
          goOn <- modifyMVar st $ \s@(ServerState (Clients clients _) _ _ _ _ _ signal) ->
            tryReadMVar signal >>= maybe
              (return (s, False))
              (\thisWorld -> do
                if currentWorld == thisWorld
                  then do
                    let players = onlyPlayers' $ Map.elems clients
                        zero = zeroCoords
                        accs =
                          mapMaybe
                            (\p -> let acc = getShipAcceleration p
                                   in if acc == zero
                                        then Nothing
                                        else Just (getClientId $ getIdentity p, acc))
                            players
                        evt = GameEvent $ PeriodicMotion accs []
                    sendClients' evt players
                    s' <- flip execStateT s $ modifyClients $ \p -> p { getShipAcceleration = zeroCoords }
                    return (s', True)
                  else
                    return (s, False)) -- no need to zero ships accelerations, it is done when starting the level.
          when goOn $ go $ Just update
    go Nothing
  -- on game start, initialize acceleration accumulators to 0.
  -- every gamePeriod, send accumulators value and reset them to 0.


modifyClient :: ClientId -> (Client -> Client) -> StateT ServerState IO ()
modifyClient cId f =
  modify $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.adjust f (getClientId cId) $ getClients' clients
                    }
          }

modifyClients :: (Client -> Client) -> StateT ServerState IO ()
modifyClients f =
  modify $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.map f $ getClients' clients
                    }
          }

requestWorld :: StateT ServerState IO ()
requestWorld = do
  incrementWorldId
  get >>= \(ServerState (Clients clients _) _ (LevelSpec level _ _) params wid _ _) -> do
    shipIds <- map getIdentity <$> onlyPlayers
    liftIO $ flip sendFirstWorldBuilder clients $
      WorldRequest $ WorldSpec level shipIds params wid
 where
  incrementWorldId =
    modify $ \s ->
      let wid = maybe (WorldId 0) succ $ getLastRequestedWorldId' s
      in s { getLastRequestedWorldId' = Just wid }
  sendFirstWorldBuilder evt =
    sendClients' evt . take 1 . filter ((== WorldCreator) . getCapability . getClientType) . Map.elems


onChangeWorldParams :: (WorldParameters -> WorldParameters)
                    -> StateT ServerState IO ()
onChangeWorldParams f = do
  modify $ \s ->
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
