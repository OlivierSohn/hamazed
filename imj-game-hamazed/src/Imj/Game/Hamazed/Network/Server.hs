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

import           Control.Concurrent.MVar (MVar, modifyMVar_, modifyMVar)
import           Control.Monad (forever)
import           Control.Monad.Reader(runReaderT, ask)
import           Control.Monad.State.Strict(runStateT, StateT, modify, get, state)
import           Data.Char (isPunctuation, isSpace, toLower)
import           Data.Text(Text, pack)
import           Data.Tuple(swap)
import           Network.WebSockets
                  (ServerApp, PendingConnection, sendBinaryData, receiveData, acceptRequest, forkPingThread
                  , WebSocketsData(..), Connection, DataMessage(..), sendDataMessage)
import           UnliftIO.Exception (finally)

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Class.ClientNode


defaultPort :: ServerPort
defaultPort = ServerPort 10051

mkServer :: (Maybe ServerName) -> ServerPort -> Server
mkServer Nothing = Local
mkServer (Just (ServerName n)) =
  Distant $ ServerName $ map toLower n

removeClient :: ClientId -> Clients -> Clients
removeClient id' c =
  c { getClients' = removeClient' id' id $ getClients' c }

removeClient' :: ClientId -> (a -> Client) -> [a] -> [a]
removeClient' id' f =
  filter ((/= id') . getIdentity . f)

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
  liftIO . sendClients' evt . (map getClient) =<< getPlayers

appSrv :: MVar ServerState -> ServerApp
appSrv s pending = runReaderT (appSrv' pending) s

type ServerM = ReaderT (MVar ServerState) IO

modifyState :: (ServerState -> IO (ServerState, a))
            -> ServerM a
modifyState modAction = ask >>= liftIO . (flip modifyMVar) modAction

modifyState_ :: (ServerState -> IO (ServerState))
            -> ServerM ()
modifyState_ modAction = ask >>= liftIO . (flip modifyMVar_) modAction

appSrv' :: PendingConnection -> ServerM ()
appSrv' pending = do
  conn <- liftIO $ acceptRequest pending
  liftIO $ forkPingThread conn 30 -- to keep the connection alive (TODO should we use keepalive socket property instead?)
  msg <- liftIO $ receiveData conn
  case msg of
    Connect sn@(SuggestedPlayerName suggestedName) cliType -> do
      let nameError =
           any ($ suggestedName)
            [ null
            , any isPunctuation
            , any isSpace]
      if nameError
        then
          liftIO $ sendBinaryData conn $ ConnectionRefused $
            InvalidName sn ("Name cannot " <>
              "contain punctuation or whitespace, and " <>
              "cannot be empty" :: Text)
        else do
          client <-
            modifyState $ \s -> swap <$> runStateT (makeClient conn sn cliType) s
          flip finally
            (modifyState_ $ \s -> snd <$> runStateT (disconnect $ getIdentity client) s) $
            forever $
              liftIO (receiveData conn) >>= \evt ->
                modifyState_ $ \s -> snd <$> runStateT (handleIncomingEvent client evt) s
    _ -> error $ "first received msg is not Connect : " ++ show msg


makeClient :: Connection -> SuggestedPlayerName -> ClientType -> StateT ServerState IO Client
makeClient conn sn cliType =
  takeShipId >>= \sid ->
    makePlayerName sn >>= \name -> do
      let cId = ClientId name sid
          client = Client cId conn cliType
      addClient client
      allClients >>= \clients ->
        send client $ ConnectionAccepted cId $ map (getPlayerName . getIdentity) clients
      sendClients $ PlayerInfo cId Joins
      return client
 where
  takeShipId =
    state $ \s ->
      let newShipId = succ $ getNextShipId $ getClients s
      in ( newShipId, s { getClients = (getClients s) { getNextShipId = newShipId } } )
  addClient c =
    modify $ \ s@(ServerState cls@(Clients clients _) _ _ _ _ _ _) ->
      s { getClients = cls { getClients' = c : clients } }



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

disconnect :: ClientId -> StateT ServerState IO ()
disconnect x = do
  modify $ \s ->
    s { getClients        = removeClient x $ getClients s
      , getPlayingClients = removeClient' x getClient $ getPlayingClients s
      }
  sendClients $ PlayerInfo x Leaves

allClients :: StateT ServerState IO [Client]
allClients = getClients' . getClients <$> get

getIntent :: StateT ServerState IO Intent
getIntent = getIntent' <$> get

getPlayers :: StateT ServerState IO [Player]
getPlayers = getPlayingClients <$> get

getLastRequestedWorldId :: StateT ServerState IO (Maybe WorldId)
getLastRequestedWorldId = getLastRequestedWorldId' <$> get

error' :: Client -> String -> StateT ServerState IO ()
error' client txt = do
  send client $ Error txt
  error txt -- this error may not be very readable if another thread writes to the console,
    -- hence we sent the error to the client, so that it can error too.

handleIncomingEvent :: Client -> ClientEvent -> StateT ServerState IO ()
handleIncomingEvent client@(Client cId _ _) = \case
  Connect (SuggestedPlayerName sn) _ ->
     error' client $ "already connected : " <> sn
  Disconnect -> do
    disconnect cId
    send client DisconnectionAccepted
  Say what ->
    sendClients $ PlayerInfo cId $ Says what
  -- TODO are "EnteredState" events are useful?
  EnteredState Excluded -> return ()
  EnteredState PlayLevel -> return ()
  ExitedState Excluded -> getIntent >>= \case
    Intent'Setup ->
      getPlayers >>= \players -> do
        -- add client to playing clients if it is not already a player
        let isPlayer = any ((== cId) . getIdentity . getClient) players
        unless isPlayer $ modify $ \s ->
          s { getPlayingClients = mkPlayer client:getPlayingClients s }
          -- request world. (-> next step when client 'IsReady')
        requestWorld
    _ -> send client $ EnterState Excluded
  ExitedState PlayLevel -> return ()
  GameEnded outcome -> do
    getIntent >>= \case
      Intent'PlayGame ->
        modify $ \s -> s { getIntent' = Intent'GameEnd outcome }
      Intent'GameEnd o ->
        if o == outcome
          then
            return ()
          else
            error $ "inconsistent outcomes:" ++ show (o, outcome)
      _ -> error "logic"
    modify $ \s ->
      s { getPlayingClients = setFinished cId $ getPlayingClients s }
    getPlayers >>= \players ->
      when (all finished players) $ do
        modify $ \s -> s { getLevelSpec = mkLevelSpec firstLevel }
        requestWorld
        sendClients $ GameInfo $ GameResult outcome
  LevelWon -> error' client "todo"
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
    updatePlayerWorldId cId wid
    getIntent >>= \case
      Intent'PlayGame ->
        getPlayers >>= \players ->
          getLastRequestedWorldId >>= maybe
            (return ())
            (\lastWId -> do
              -- start the game when all players have the right world
              let ready = ((==) (Just lastWId)) . getCurrentWorld
              when (all ready players) $ do
                modify $ \s ->
                  s { getPlayingClients = setStartPlay $ getPlayingClients s }
                sendPlayers $ EnterState PlayLevel)
      Intent'Setup ->
        -- (follow-up from 'ExitedState Excluded')
        -- Allow the client to setup the world, now that the world contains its ship.
        send client $ EnterState Setup
      Intent'GameEnd _ -> return ()
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
  Action _ _ -> return () -- TODO on laser event : what if the client sees the previous game step?

gameScheduler :: MVar ServerState -> IO ()
gameScheduler _s = --forever $ do
  -- on game start, initialize acceleration accumulators to 0.
  -- pause / run
  -- every gamePeriod, send accumulators value and reset them to 0.
  return ()

setStartPlay :: [Player] -> [Player]
setStartPlay = map (\p -> p { getState = InGame })

finished :: Player -> Bool
finished = ((==) Finished) . getState

setFinished :: ClientId -> [Player] -> [Player]
setFinished i = map (\p -> if (getIdentity . getClient) p == i
                            then p { getState = Finished}
                            else p)

updatePlayerWorldId :: ClientId -> WorldId -> StateT ServerState IO ()
updatePlayerWorldId cId wid =
  modify $ \s ->
    s { getPlayingClients =
          map (\p ->
                  if (cId == (getIdentity . getClient) p)
                    then p { getCurrentWorld = Just wid }
                    else p)
              $ getPlayingClients s
      }

requestWorld :: StateT ServerState IO ()
requestWorld = do
  incrementWorldId
  get >>= \(ServerState clients playing _ (LevelSpec level _ _) params wid _) -> do
    let shipIds = map (getClientId . getIdentity . getClient) playing
    liftIO $ flip sendFirstWorldBuilder clients $
      WorldRequest $ WorldSpec level shipIds params wid
 where
  incrementWorldId =
    modify $ \s ->
      let wid = maybe (WorldId 0) succ $ getLastRequestedWorldId' s
      in s { getLastRequestedWorldId' = Just wid }
  sendFirstWorldBuilder evt =
    sendClients' evt . take 1 . filter ((== WorldCreator) . getClientType) . getClients'


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
