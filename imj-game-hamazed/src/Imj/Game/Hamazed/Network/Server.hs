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

worldBuilders :: [Client] -> [Client]
worldBuilders = filter ((== WorldCreator) . getClientType)

shipIds :: [Client] -> [ShipId]
shipIds = map (getClientId . getIdentity)

removeClient :: ClientId -> Clients -> Clients
removeClient id' c =
  c { getClients' = removeClient' id' id $ getClients' c }

removeClient' :: ClientId -> (a -> Client) -> [a] -> [a]
removeClient' id' f =
  filter ((/= id') . getIdentity . f)

chatBroadcast :: ClientId -> PlayerNotif -> [Client] -> IO ()
chatBroadcast (ClientId player _) n =
  sendClients (PlayerInfo player n)

{-# INLINABLE sendNBinaryData #-}
sendNBinaryData :: WebSocketsData a => [Connection] -> a -> IO ()
sendNBinaryData conns a =
  let serialized = Binary $ toLazyByteString a
  in mapM_ (flip sendDataMessage serialized) conns

sendClient :: ServerEvent -> Client -> IO ()
sendClient evt = flip sendBinaryData evt . getConnection

sendClients :: ServerEvent -> [Client] -> IO ()
sendClients evt = flip sendNBinaryData evt . map getConnection

sendPlayers :: ServerEvent -> StateT ServerState IO ()
sendPlayers evt =
  fmap (map fst) getPlayers >>= liftIO . sendClients evt

sendFirstWorldBuilder :: ServerEvent -> Clients -> IO ()
sendFirstWorldBuilder evt =
  sendClients evt . take 1 . worldBuilders . getClients'

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
          client@(Client clId _ _ _) <-
            modifyState $ \s -> swap <$> runStateT (makeClient conn sn cliType) s
          flip finally
            (modifyState_ $ \s -> snd <$> runStateT (disconnect clId) s) $
            forever $
              liftIO (receiveData conn) >>= \evt ->
                modifyState_ $ \s -> snd <$> runStateT (handleIncomingEvent client evt) s
    _ -> error $ "first received msg is not Connect : " ++ show msg

takeShipId :: StateT ServerState IO ShipId
takeShipId =
  state $ \s ->
    let newShipId = succ $ getNextShipId $ getClients s
    in ( newShipId, s { getClients = (getClients s) { getNextShipId = newShipId } } )

addClient :: Client -> StateT ServerState IO ()
addClient c =
  modify $ \ s@(ServerState cls@(Clients clients _) _ _ _ _ _ _) ->
    s { getClients = cls { getClients' = c : clients } }

makeClient :: Connection -> SuggestedPlayerName -> ClientType -> StateT ServerState IO Client
makeClient conn sn cliType =
  takeShipId >>= \sid ->
    makePlayerName sn >>= \name -> do
      let cId = ClientId name sid
          client = mkClient conn cliType cId
      addClient client
      allClients >>= \clients ->
        liftIO $ do
          flip sendClient client
            (ConnectionAccepted cId $ map (getPlayerName . getIdentity) clients)
          sendClients (PlayerInfo name Joins) clients
      return client

makePlayerName :: SuggestedPlayerName -> StateT ServerState IO PlayerName
makePlayerName (SuggestedPlayerName sn) = do
  let go mayI = do
        let proposal = PlayerName $ pack $ maybe sn ((++) sn . show) mayI
        playerNameExists proposal >>= \case
          True -> go $ Just $ maybe (2::Int) succ mayI
          False -> return proposal
  go Nothing

disconnect :: ClientId -> StateT ServerState IO ()
disconnect x = do
  modify $ \s ->
    s { getClients        = removeClient x $ getClients s
      , getPlayingClients = removeClient' x fst $ getPlayingClients s
      }
  allClients >>= liftIO . chatBroadcast x Leaves

allClients :: StateT ServerState IO [Client]
allClients = getClients' . getClients <$> get

getIntent :: StateT ServerState IO Intent
getIntent = getIntent' <$> get

getPlayers :: StateT ServerState IO [Player]
getPlayers = getPlayingClients <$> get

error' :: Client -> String -> StateT ServerState IO ()
error' c txt = do
  liftIO $ sendClient (Error txt) c
  error txt -- this error may not be very readable if another thread writes to the console,
    -- hence we sent the error to the client, so that it can error too.

handleIncomingEvent :: Client -> ClientEvent -> StateT ServerState IO ()
handleIncomingEvent cl@(Client c _ _ _) = \case
  Connect (SuggestedPlayerName sn) _ ->
     error' cl $ "already connected : " <> sn
  Disconnect -> do
    disconnect c
    liftIO $ sendClient DisconnectionAccepted cl
  Say what ->
    allClients >>= liftIO . chatBroadcast c (Says what)
  -- TODO are "EnteredState" events are useful?
  EnteredState Excluded -> return ()
  EnteredState PlayLevel -> return ()
  ExitedState Excluded -> do
    getIntent >>= \case
      Intent'Setup ->
        getPlayers >>= \players -> do
          -- add client to playing clients if it is not already a player
          let isPlayer = any ((== c) . getIdentity . fst) players
          unless isPlayer $ modify $ \s ->
            s { getPlayingClients = (cl, Nothing):getPlayingClients s }
            -- request world. (-> next step when client 'IsReady')
          requestWorld
      _ -> liftIO $ sendClient (EnterState Excluded) cl
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
      let remainPlaying = removeClient' c fst $ getPlayingClients s
      in s { getPlayingClients = remainPlaying }
    getPlayers >>= \players ->
      when (null players) $ do
        modify $ \s -> s { getLevelSpec = mkLevelSpec firstLevel }
        requestWorld
        allClients >>= liftIO . sendClients (GameInfo $ GameResult outcome)
  LevelWon -> error' cl "todo"
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
        allClients >>= liftIO . chatBroadcast c StartsGame
        sendPlayers $ ExitState Setup -- prevent other playing clients from modifying the world parameters
        requestWorld
      _ -> return ()
------------------------------------------------------------------------------
-- A 'WorldCreator' client may be requested to propose a world:
------------------------------------------------------------------------------
  (WorldProposal essence) ->
    get >>= \s -> do
      let wid = fromMaybe (error "Nothing WorldId in WorldProposal") $ getWorldId essence
      when (getLastRequestedWorldId s == Just wid) $ do
        sendPlayers $ ChangeLevel (getLevelSpec s) essence
------------------------------------------------------------------------------
-- Clients notify when they have received a given world, and the corresponding
--   UI Animation is over (hence the level shall start, for example.):
------------------------------------------------------------------------------
  IsReady wid -> do
    -- update 'WorldId' in corresponding playing clients
    modify $ \s@(ServerState _ playingClients _ _ _ _ _) ->
      let newPlaying = map (\p@(cl'@(Client c' _ _ _), _) ->
                              if c' == c
                                then (cl', Just wid)
                                else p)
                            playingClients
      in s { getPlayingClients = newPlaying }
    getIntent >>= \case
      Intent'PlayGame -> getPlayers >>= \players -> do
        -- start the game when all players have the right world
        let ready (_,wid') = Just wid == wid'
        when (all ready players) $ sendPlayers $ EnterState PlayLevel
      Intent'Setup -> do
        -- (follow-up from 'ExitedState Excluded')
        -- Allow the client to setup the world, now that the world contains its ship.
        liftIO $ sendClient (EnterState Setup) cl
      Intent'GameEnd _ -> return ()
------------------------------------------------------------------------------
-- Clients in 'PlayLevel' state can play the game.
--
--   [If /any/ client is in 'PlayLevel' state, /no/ client is in 'Setup' state]
------------------------------------------------------------------------------
  Action _ _ -> return () -- TODO on laser event : what if the client sees the previous game step?

requestWorld :: StateT ServerState IO ()
requestWorld = do
  nextWorldId
  get >>= \(ServerState clients playing _ (LevelSpec level _ _) params wid _) ->
    liftIO $ flip sendFirstWorldBuilder clients $
      WorldRequest $ WorldSpec level (shipIds $ map fst playing) params wid

nextWorldId :: StateT ServerState IO ()
nextWorldId =
  modify $ \s ->
    let wid = maybe (WorldId 0) succ $ getLastRequestedWorldId s
    in s { getLastRequestedWorldId = Just wid }

playerNameExists :: PlayerName -> StateT ServerState IO Bool
playerNameExists name =
  allClients >>= \l ->
    return $ any ((== name) . getPlayerName . getIdentity ) l


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
