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

import           Control.Concurrent.MVar (MVar, modifyMVar_, modifyMVar, readMVar)
import           UnliftIO.Exception (finally)
import           Control.Monad (forever)
import           Control.Monad.Reader(runReaderT, ask)
import           Data.Char (isPunctuation, isSpace, toLower)
import           Data.Text(Text, pack)
import           Network.WebSockets
                  (ServerApp, PendingConnection, sendBinaryData, receiveData, acceptRequest, forkPingThread)

import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Class.ClientNode


defaultPort :: ServerPort
defaultPort = ServerPort 10051

mkServer :: (Maybe ServerName) -> ServerPort -> Server
mkServer Nothing = Local
mkServer (Just (ServerName n)) = case map toLower n of
  "localhost" -> Local
  "127.0.0.1" -> Local
  ln -> Distant (ServerName ln)

playerNameExists :: PlayerName -> Clients -> Bool
playerNameExists name (Clients l _) =
  any ((== name) . getPlayerName . getIdentity ) l

worldBuilders :: [Client] -> [Client]
worldBuilders = filter ((== WorldCreator) . getClientType)

shipIds :: [Client] -> [ShipId]
shipIds = map (getClientId . getIdentity)

addClient :: Client -> Clients -> Clients
addClient client c@(Clients clients _) =
  c { getClients' = client : clients }

removeClient :: ClientId -> Clients -> Clients
removeClient id' c =
  c { getClients' = removeClient' id' id $ getClients' c }

removeClient' :: ClientId -> (a -> Client) -> [a] -> [a]
removeClient' id' f =
  filter ((/= id') . getIdentity . f)

chatBroadcast :: ClientId -> PlayerNotif -> ServerM ()
chatBroadcast (ClientId player _) n =
  broadcastAll (Info player n)

broadcastAll :: ServerEvent -> ServerM ()
broadcastAll evt =
  readState >>= mapM_ (liftIO . sendClient evt) . getClients' . getClients

-- only to playing clients
broadcast :: ServerEvent -> ServerM ()
broadcast evt =
  readState >>= mapM_ (liftIO . sendClient evt . fst) . getPlayingClients

sendClient :: ServerEvent -> Client -> IO ()
sendClient evt = flip sendBinaryData evt . getConnection

sendFirstWorldBuilder :: ServerEvent -> Clients -> IO ()
sendFirstWorldBuilder evt =
  mapM_ (sendClient evt) . take 1 . worldBuilders . getClients'

appSrv :: MVar ServerState -> ServerApp
appSrv state pending = runReaderT (appSrv' pending) state

readState :: ServerM ServerState
readState = ask >>= liftIO . readMVar

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
  clients <- getClients <$> readState
  case msg of
    Connect sn@(SuggestedPlayerName suggestedName) cliType -> case suggestedName of
      _ | any ($ suggestedName)
            [ null
            , any isPunctuation
            , any isSpace] ->
            liftIO $ sendBinaryData conn $ ConnectionRefused $
              InvalidName sn ("Name cannot " <>
                "contain punctuation or whitespace, and " <>
                "cannot be empty" :: Text)
        | playerNameExists (PlayerName $ pack suggestedName) clients -> -- TODO change name
            liftIO $ sendBinaryData conn $ ConnectionRefused $
              InvalidName sn ("Player already exists" :: Text)
        | otherwise -> do
            shipId <- modifyState $ \s -> do
                let c@(Clients _ sid) = getClients s
                return (s { getClients = c { getNextShipId = succ sid } }, sid)
            let playerId = ClientId (PlayerName $ pack suggestedName) shipId
            let client = mkClient conn cliType playerId
            flip finally (disconnect playerId) $ do
              registerClient client
              -- don't request a new world as a game may be in progress.
              chatBroadcast playerId Joins
              handleIncomingEvents client
    _ -> error $ "first received msg is not Connect : " ++ show msg

registerClient :: Client -> ServerM ()
registerClient client = do
  modifyState_ $ \s -> do
    let newClients@(Clients l _) = addClient client $ getClients s
        s' = s { getClients = newClients }
    flip sendClient client $
      ConnectionAccepted (getIdentity client) $ map (getPlayerName . getIdentity) l
    return s'

disconnect :: ClientId -> ServerM ()
disconnect x = do
  modifyState_ $ \s ->
    return s { getClients        = removeClient x $ getClients s
             , getPlayingClients = removeClient' x fst $ getPlayingClients s }
  chatBroadcast x Leaves

handleIncomingEvents :: Client -> ServerM ()
handleIncomingEvents cl@(Client c _ conn _) =
  forever $ liftIO (receiveData conn) >>= \case
    Connect (SuggestedPlayerName sn) _ ->
      error $ "already connected : " <> sn
    Disconnect -> do
      disconnect c
      liftIO $ sendClient DisconnectionAccepted cl
    Say what ->
      chatBroadcast c $ Says what
    (EnteredState Excluded) -> error "todo" -- TODO are "EnteredState" events are useful?
    (EnteredState Play) -> error "todo"
    (ExitedState Excluded) -> error "todo"
    ExitedState Play -> return () -- don't act : another more specific event will follow:
    GameEnded _outcome -> error "todo"
    LevelWon -> error "todo"
  ------------------------------------------------------------------------------
  -- Clients in 'Setup' state can configure the world.
  --
  --   [If /any/ client is in 'Setup' state, /no/ client is in 'Play' state]
  ------------------------------------------------------------------------------
    EnteredState Setup ->
      modifyState_ $ \s ->
        return s { getPlayingClients = (cl, Nothing):getPlayingClients s }
    ChangeWallDistribution t ->
      onChangeWorldParams $ changeWallDistrib t
    ChangeWorldShape s ->
      onChangeWorldParams $ changeWorldShape s
    ExitedState Setup ->
        -- TODO Under contention, an optimistic concurrency approach (TMVar + STM)
        -- could perform better than this lock-based approach.
      modifyState (\s ->
        case getIntent s of
          Setup -> return (s { getIntent = Play }, Just s)
          _ -> return(s, Nothing))
        >>= maybe (return ()) (\s' -> do
          chatBroadcast c $ StartsGame
          broadcast $ ExitState Setup -- prevent other playing clients from modifying the world parameters
          withUniqueWorldId $ requestWorld s')
  ------------------------------------------------------------------------------
  -- A 'WorldCreator' client may be requested to propose a world:
  ------------------------------------------------------------------------------
    (WorldProposal essence) -> readState >>= \s -> do
      let wid = fromMaybe (error "Nothing WorldId in WorldProposal") $ getWorldId essence
      when (getLastRequestedWorldId s == Just wid) $ do
        broadcast $ flip ChangeLevel essence $ getLevelSpec s
  ------------------------------------------------------------------------------
  -- Clients notify when they have received a given world, and the corresponding
  --   UI Animation is over (hence the level shall start, for example.):
  ------------------------------------------------------------------------------
    IsReady wid -> do
      (players, intent) <- modifyState $ \s@(ServerState _ playingClients _ _ _ _ intent) -> do
        let newPlaying = map (\p@(cl'@(Client c' _ _ _), _) ->
                                if c' == c
                                  then (cl', Just wid)
                                  else p) playingClients
        return (s { getPlayingClients = newPlaying }, (newPlaying, intent) )
      let ready (_,wid') = Just wid == wid'
      when (intent == Play && all ready players) $ broadcast $ EnterState Play
  ------------------------------------------------------------------------------
  -- Clients in 'Play' state can play the game.
  --
  --   [If /any/ client is in 'Play' state, /no/ client is in 'Setup' state]
  ------------------------------------------------------------------------------
    Action _ _ -> return () -- TODO on laser event : what if the client sees the previous game step?

requestWorld :: ServerState -> WorldId -> IO ()
requestWorld (ServerState clients playing _ (LevelSpec level _ _) params _ _) =
  flip sendFirstWorldBuilder clients .
    WorldRequest . WorldSpec level (shipIds $ map fst playing) params . Just

nextWorldId :: ServerM WorldId
nextWorldId =
  modifyState (\s@(ServerState _ _ _ _ _ prevWid _) -> do
    let wid = maybe (WorldId 0) succ prevWid
    return (s { getLastRequestedWorldId = Just wid }, wid))

type ServerM = ReaderT (MVar ServerState) IO

withUniqueWorldId :: (WorldId -> IO ())
                  -> ServerM ()
withUniqueWorldId act =
  nextWorldId >>= liftIO . act

onChangeWorldParams :: (WorldParameters -> WorldParameters)
                    -> ServerM ()
onChangeWorldParams f = do
  modifyState (\s ->
    let s' = s { getWorldParameters = f $ getWorldParameters s }
    in return (s',s'))
    >>= withUniqueWorldId . requestWorld

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

-- state transitions
{-
(in Update)
  StartGame            -> putUserIntent Play >> updateAppState (Right $ NextLevel firstLevel)

onStartLevel :: (MonadState AppState m, MonadIO m)
             => Int -> m ()
onStartLevel n =
  getGame >>= \(Game _ params state@(GameState _ _ (World _ ships _ _) _ _ _ _ (Screen sz _)) _ _ _ _) ->
    mkInitialState params sz n (map getShipId ships) (Just state) >>= putGameState

onEndGame :: (MonadState AppState m, Canvas e, MonadReader e m, MonadIO m) => m ()
onEndGame = do
  putUserIntent Configure
  getGameParameters >>= \params -> do
    getWorld >>= \(World _ ships _ _) -> do
      getTargetSize
        >>= liftIO . initialGameState params (map getShipId ships)
        >>= putGameState
      return ()
-}
