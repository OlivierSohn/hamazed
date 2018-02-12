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
mkServer (Just (ServerName n)) =
  Distant $ ServerName $ map toLower n

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
        | otherwise -> do
            client@(Client clId _ _ _) <- modifyState $ \s -> do
                let c@(Clients _ sid) = getClients s
                    makeUniqueName mayI =
                      let proposal = PlayerName $ pack $ maybe suggestedName ((++) suggestedName . show) mayI
                      in if playerNameExists proposal c
                          then
                            makeUniqueName $ Just $ maybe (2::Int) succ mayI
                          else
                            proposal
                    name = makeUniqueName Nothing
                    cId = ClientId name sid
                    client = mkClient conn cliType cId
                    newClients = addClient client c
                    s' = s { getClients = newClients { getNextShipId = succ sid } }
                flip sendClient client
                  (ConnectionAccepted cId $ map (getPlayerName . getIdentity) $ getClients' $ newClients)
                mapM_ (sendClient (Info name Joins)) $ getClients' $ newClients
                return (s', client)
            flip finally (disconnect clId) $ handleIncomingEvents client
    _ -> error $ "first received msg is not Connect : " ++ show msg


disconnect :: ClientId -> ServerM ()
disconnect x = do
  modifyState_ $ \s ->
    return s { getClients        = removeClient x $ getClients s
             , getPlayingClients = removeClient' x fst $ getPlayingClients s }
  chatBroadcast x Leaves

error' :: Client -> String -> ServerM ()
error' c txt = do
  liftIO $ sendClient (Error txt) c
  error txt -- this error may not be very readable if another thread writes to the console,
    -- hence we sent the error to the client, so that it can error too.

handleIncomingEvents :: Client -> ServerM ()
handleIncomingEvents cl@(Client c _ conn _) =
  forever $ liftIO (receiveData conn) >>= \case
    Connect (SuggestedPlayerName sn) _ ->
       error' cl $ "already connected : " <> sn
    Disconnect -> do
      disconnect c
      liftIO $ sendClient DisconnectionAccepted cl
    Say what ->
      chatBroadcast c $ Says what
    -- TODO are "EnteredState" events are useful?
    EnteredState Excluded -> return ()
    EnteredState PlayLevel -> return ()
    ExitedState Excluded -> do
      getIntent <$> readState >>= \case
        Intent'Setup -> do
          -- add client to playing clients if it is not already there
          modifyState (\s ->
            let alreadyThere = any ((== c) . getIdentity . fst) $ getPlayingClients s
                s' = if alreadyThere
                        then s
                        else s { getPlayingClients = (cl, Nothing):getPlayingClients s }
            in return (s',s'))
            -- request world. (-> next step when client 'IsReady')
            >>= withUniqueWorldId . requestWorld
        _ -> liftIO $ sendClient (EnterState Excluded) cl
    ExitedState PlayLevel -> return ()
    GameEnded outcome ->
      modifyState (\s ->
        let s' = case getIntent s of
              Intent'PlayGame -> s { getIntent = Intent'GameEnd outcome }
              Intent'GameEnd o ->
                if o == outcome
                  then
                    s
                  else
                    error $ "inconsistent outcome:" ++ show (o, outcome)
              _ -> error "logic"
            remainPlaying = removeClient' c fst $ getPlayingClients s'
            s'' = s' { getPlayingClients = remainPlaying }
        in return (s'', if null remainPlaying
                          then Just s''
                          else Nothing))
        >>= maybe (return ()) (\st -> do
          mapM_ (flip chatBroadcast $ GameResult outcome) $ map (getIdentity . fst) $ getPlayingClients st
          modifyState (\s -> let s' = s { getLevelSpec = mkLevelSpec firstLevel } in return (s',s'))
            >>= withUniqueWorldId . requestWorld)
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
        -- TODO Under contention, an optimistic concurrency approach (TMVar + STM)
        -- could perform better than this lock-based approach.
      modifyState (\s ->
        case getIntent s of
          Intent'Setup -> return (s { getIntent = Intent'PlayGame }, Just s)
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
      -- update 'WorldId' in corresponding playing clients
      (players, intent) <- modifyState $ \s@(ServerState _ playingClients _ _ _ _ intent) -> do
        let newPlaying = map (\p@(cl'@(Client c' _ _ _), _) ->
                                if c' == c
                                  then (cl', Just wid)
                                  else p) playingClients
        return (s { getPlayingClients = newPlaying }, (newPlaying, intent) )
      let ready (_,wid') = Just wid == wid'
      case intent of
        Intent'PlayGame ->
          -- start the game when all players have the right world
          when (all ready players) $ broadcast $ EnterState PlayLevel
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
