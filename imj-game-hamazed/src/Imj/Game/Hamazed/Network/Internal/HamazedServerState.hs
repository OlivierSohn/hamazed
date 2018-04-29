{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Hamazed.Network.Internal.HamazedServerState
      ( HamazedServerState(..)
      -- * utilities
      , onlyPlayersMap
      , gets'
      , requestWorld
      , requestWorldBy
      , cancelWorldRequest
      , notifyPlayersN
      , notifyPlayers
      , updateCurrentStatus
      ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay)
import           Control.Concurrent.MVar.Strict(MVar)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader(asks)
import           Control.Monad.State.Strict(MonadState, modify', gets, get, state, runStateT)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe(isJust)
import qualified Data.Set as Set
import           Data.Text(pack, unpack)
import qualified Data.Text as Text(intercalate)
import           Data.Tuple(swap)
import           UnliftIO.MVar (modifyMVar, swapMVar, readMVar, tryReadMVar, tryTakeMVar, putMVar)

import           Imj.ClientServer.Class
import           Imj.ClientServer.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Types
import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.ClientId
import           Imj.Game.Hamazed.Network.Setup
import           Imj.Graphics.Color.Types
import           Imj.Server.Types

import           Imj.ClientServer.Run
import           Imj.Game.Hamazed.Chat
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.Text.ColorString(colored)
import           Imj.Music hiding(Do)
import           Imj.Server.Connection
import           Imj.Server
import           Imj.Server.Log


-- | 'HamazedServerState' handles a single game.
data HamazedServerState = HamazedServerState {
    gameTiming :: !GameTiming -- could / should this be part of CurrentGame?
  , levelSpecification :: {-# UNPACK #-} !LevelSpec
  , worldParameters :: {-# UNPACK #-} !WorldParameters
  -- ^ The actual 'World' is stored on the 'Clients'
  , worldCreation :: {-unpack sum-} !WorldCreation
  , intent :: {-unpack sum-} !Intent
  -- ^ Influences the control flow (how 'ClientEvent's are handled).
  , centerColor :: {-# UNPACK #-} !(Color8 Foreground)
  -- ^ The color scheme.
  , scheduledGame :: {-# UNPACK #-} !(MVar CurrentGame)
  -- ^ When set, it informs the scheduler thread that it should run the game.
} deriving(Generic)
instance NFData HamazedServerState
instance ClientServer HamazedServerState where

  type ClientT HamazedServerState = HamazedClient
  type ConnectIdT HamazedServerState = SuggestedPlayerName
  type ServerEventT HamazedServerState = HamazedServerEvent
  type ClientEventT HamazedServerState = HamazedClientEvent
  -- Where 'Set ClientId' represents the players in the current game:
  type ReconnectionContext HamazedServerState = Set ClientId

  inParallel = [gameScheduler]

  acceptConnection (SuggestedPlayerName name) = checkName name

--  tryReconnect :: SuggestedPlayerName -> StateT (ServerState HamazedServerState) IO (Maybe (ClientId, ReonnectionContext HamazedServerState))
  tryReconnect _ =
    gets' scheduledGame >>= liftIO . tryReadMVar >>= maybe
      (do serverLog $ pure "No game is running"
          return Nothing)
      (\(CurrentGame _ gamePlayers status _) -> case status of
          (Paused _ _) -> do
          -- we /could/ use 'Paused' ignored argument but it's probably out-of-date
          -- - the game scheduler thread updates it every second only -
          -- so we recompute the difference:
            connectedPlayers <- gets onlyPlayersMap
            let connectedPlayerKeys = Map.keysSet connectedPlayers
                mayDisconnectedPlayerKey = Set.lookupMin $ Set.difference gamePlayers connectedPlayerKeys
                gameConnectedPlayers = Map.keysSet $ Map.restrictKeys connectedPlayers gamePlayers
            maybe
              (do serverLog $ pure "A paused game exists, but has no disconnected player."
                  return Nothing)
              (\disconnected -> do
                  serverLog $ (\i -> "A paused game exists, " <> i <> " is disconnected.") <$> showId disconnected
                  return $ Just (disconnected, gameConnectedPlayers))
              mayDisconnectedPlayerKey
          _ -> do serverLog $ pure "A game is in progress."
                  return Nothing)

  createClient i sn =
    mkHamazedClient <$> makePlayerName sn <*> fmap (mkClientColorFromCenter i) (gets' centerColor)

  eventsOnWillAddClient i (HamazedClient name _ _ _ _ color) =
    map (RunCommand i) [AssignName name , AssignColor color]

  onReconnection gameConnectedPlayers =
    gets' worldCreation >>= \(WorldCreation creationSt wid _ _) -> case creationSt of
      Created ->
        -- Since we are in a reconnection scenario, the game is paused, and a world was created,
        -- so we ask one of the connected players to send it si that the newcomer can get it.
        gets clientsMap >>= notifyN [WorldRequest wid GetGameState] . Map.take 1 . flip Map.restrictKeys gameConnectedPlayers
      CreationAssigned _ ->
        -- A game is in progress and a "next level" is being built:
        -- add the newcomer to the assigned builders.
        participateToWorldCreation wid

  greetings = do
    presentClients <-
      Map.map ((\cl -> PlayerEssence (getName cl) Present $ getColor cl) . unClient) <$> gets clientsMap
    wp <- gets' worldParameters
    return [ OnWorldParameters wp, MeetThePlayers presentClients]

  handleClientEvent = handleEvent

  afterClientLeft cid = \case
    ClientShutdown r -> do
      notifyEveryone $ RunCommand cid $ Leaves r
      unAssign cid
      gets' intent >>= \case
        IntentSetup -> requestWorld -- because the number of players has changed
        IntentPlayGame _ -> return () -- don't create a new world while a game is in progress!
    ServerShutdown _ ->
      return () -- no need to notify other clients, as they will be diconnected too,
                -- hence they will receive a server shutdown notification.


--------------------------------------------------------------------------------
-- functions used in 'inParallel' ----------------------------------------------
--------------------------------------------------------------------------------

onLevelOutcome :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
               => LevelOutcome -> m ()
onLevelOutcome outcome =
  levelNumber <$> gets' levelSpecification >>= \levelN -> do
    adjustAll (\c -> case getState c of
      Just (Playing _) -> c { getState = Just ReadyToPlay }
      _ -> c)
    notifyEveryoneN $
      (GameInfo $ LevelResult levelN outcome):
      [GameInfo GameWon | outcome == Won && levelN == lastLevel]

    modify' $ mapState $ \s ->
     if outcome == Won && levelN < lastLevel
      then -- next level
        s { levelSpecification = (levelSpecification s) { levelNumber = succ levelN }
          , intent = IntentPlayGame Nothing
          }
      else -- end game
        s { levelSpecification = (levelSpecification s) { levelNumber = firstServerLevel }
          , intent = IntentSetup
          }
    gets' intent >>= \case
      IntentSetup ->
        -- make fresh clients become players
        adjustAll' (\c -> case getState c of
          Just (Playing _) -> error "inconsistent"
          Just ReadyToPlay -> Nothing
          Nothing -> Just $ c { getState = Just ReadyToPlay })
          >>= notifyEveryoneN . map (PlayerInfo Joins) . Set.toList
      IntentPlayGame _ -> return ()

-- | To avoid deadlocks, this function should be called only from inside a
-- transaction on ServerState.
{-# INLINABLE updateCurrentStatus #-}
updateCurrentStatus :: (MonadState (ServerState HamazedServerState) m, MonadIO m)
                    => Maybe GameStatus
                    -- ^ the reference to take into account, if different from current status.
                    -> m (Maybe GameStatus)
updateCurrentStatus ref = gets' scheduledGame >>= tryReadMVar >>= maybe
  (return Nothing)
  (\(CurrentGame _ gamePlayers status _) -> do
    connectedPlayers' <- gets onlyPlayersMap
    let connectedPlayers = Map.keysSet connectedPlayers'
        missingPlayers = Set.difference gamePlayers connectedPlayers
        maybeNewStatus
          | null connectedPlayers = Just CancelledNoConnectedPlayer
          | null missingPlayers = case status of
              New -> Just $ Countdown 3 Running
              Paused _ statusBeforePause -> Just statusBeforePause
              Countdown n futureStatus -> Just $
                if n > 0
                  then
                    Countdown (pred n) futureStatus
                  else
                    futureStatus
              w@(WhenAllPressedAKey _ (Just n) _) ->
                Just $ w { countdown =
                  if n > 0
                    then Just $ pred n
                    else Nothing }
              WhenAllPressedAKey x Nothing havePressed ->
                if Map.null $ Map.filter (== False) havePressed
                  then
                    Just x
                  else
                    Nothing
              Running -> Nothing
              WaitingForOthersToEndLevel _ -> Nothing
              OutcomeValidated _ -> Nothing
              CancelledNoConnectedPlayer -> Nothing
          | otherwise = Just $ Paused missingPlayers $
              case status of
                Paused _ statusBeforePause -> statusBeforePause
                _ -> status
        newStatus = fromMaybe status maybeNewStatus
        oldStatus = fromMaybe status ref
    when (newStatus /= oldStatus) $
      onChangeStatus connectedPlayers' oldStatus newStatus
    return $ Just newStatus)

{-# INLINABLE onChangeStatus #-}
onChangeStatus :: (MonadState (ServerState HamazedServerState) m, MonadIO m)
               => Map ClientId (Client HamazedClient)
               -> GameStatus
               -> GameStatus
               -> m ()
onChangeStatus notified status newStatus = gets' scheduledGame >>= \game -> tryReadMVar game >>= maybe
  (return ())
  (\g -> do
    -- mayWorld can be concurrently modified from client handler threads, but only from
    -- inside a 'modifyMVar' on 'ServerState'. Since we are ourselves inside
    -- a 'modifyMVar' of 'ServerState', we guarantee that the 'takeMVar' inside
    -- 'swapMVar' won't block, because in the same 'modifyMVar' transaction, 'tryReadMVar'
    -- returned a 'Just'.
    liftIO $ void $ swapMVar game $! g {status' = newStatus}
    serverLog $ pure $ colored ("Game status change: " <> pack (show (status, newStatus))) yellow
    notifyN [EnterState $ PlayLevel newStatus] notified)

gameScheduler :: MVar (ServerState HamazedServerState) -> IO ()
gameScheduler st =
  readMVar st >>= \(ServerState _ _ terminate (HamazedServerState _ _ _ _ _ _ mayGame)) ->
    if terminate
      then return ()
      else
        -- block until 'scheduledGame' contains a 'CurrentGame'
        readMVar mayGame >>= \(CurrentGame refWorld _ _ _) -> do
          let run x = modifyMVar st $ \s -> fmap swap $ runStateT (run' refWorld x) s
              toActOrNotToAct act maybeRefTime continuation = run act >>= \case
                Executed dt ->
                  continuation maybeRefTime dt
                NotExecutedTryAgainLater dt -> do
                  threadDelay $ fromIntegral $ toMicros dt
                  -- retry, but forget maybeRefTime because we waited,
                  -- which introduced a discontinuity.
                  toActOrNotToAct act Nothing continuation
                NotExecutedGameCanceled -> do
                  void $ liftIO $ tryTakeMVar mayGame -- we remove the game to stop the scheduler.

          toActOrNotToAct initializePlayers Nothing $ \_ dtInit -> do
            let go mayPrevUpdate mayDt = do
                  now <- getSystemTime
                  time <- maybe
                    (return now)
                    (\dt -> do
                      let baseTime = fromMaybe now mayPrevUpdate
                          update = addDuration dt baseTime
                      threadDelay $ fromIntegral $ toMicros $ now...update
                      return update)
                    mayDt
                  toActOrNotToAct (stepWorld time) (Just time) go
            go Nothing dtInit
          gameScheduler st
 where
  -- run the action if the world matches the current world and the game was
  -- not terminated
  --run' :: WorldId
  --     -> StateT (ServerState s) IO (Maybe (Time Duration System))
  --     -> StateT (ServerState s) IO RunResult
  run' refWid act =
    go >>= \res -> do
      case res of
        NotExecutedTryAgainLater _ -> stopMusic
        NotExecutedGameCanceled -> stopMusic
        Executed _ -> return ()
      return res

   where
    stopMusic = gets unServerState >>= \(HamazedServerState _ _ _ _ _ _ game) ->
      tryTakeMVar game >>= maybe
        (return ())
        (\g@(CurrentGame _ _ _ s) -> do
          let (newScore, notesChanges) = stopScore s
          case notesChanges of
            [] -> return ()
            _:_ -> notifyPlayersN $ map (flip PlayMusic SineSynth) notesChanges
          putMVar game $ g{score = newScore})

    go = get >>= \(ServerState _ _ terminate (HamazedServerState _ _ _ _ _ _ game)) ->
      if terminate
        then do
          serverLog $ pure $ colored "Terminating game" yellow
          return NotExecutedGameCanceled
        else
          -- we use 'tryReadMVar' to /not/ block here, as we are inside a modifyMVar.
          liftIO (tryReadMVar game) >>= maybe
            (do serverError "logic : mayGame is Nothing"
                return NotExecutedGameCanceled)
            (\(CurrentGame curWid _ _ _) ->
              if refWid /= curWid
                then do
                  serverLog $ pure $ colored ("The world has changed: " <> pack (show (curWid, refWid))) yellow
                  return NotExecutedGameCanceled -- the world has changed
                else
                  updateCurrentStatus Nothing >>= maybe (return NotExecutedGameCanceled) (\case
                    Paused _ _ ->
                      return $ NotExecutedTryAgainLater $ fromSecs 1
                    CancelledNoConnectedPlayer -> do
                      onLevelOutcome $ Lost "All players left"
                      return NotExecutedGameCanceled
                    Running ->
                      Executed <$> act
                    WaitingForOthersToEndLevel _ ->
                      return $ NotExecutedTryAgainLater $ fromSecs 0.1
                    WhenAllPressedAKey _ (Just _) _ ->
                      return $ NotExecutedTryAgainLater $ fromSecs 1 -- so that the Just corresponds to seconds
                    WhenAllPressedAKey _ Nothing _ ->
                      return $ NotExecutedTryAgainLater $ fromSecs 0.1
                    Countdown _ _ ->
                      return $ NotExecutedTryAgainLater $ fromSecs 1
                    OutcomeValidated outcome -> do
                      onLevelOutcome outcome
                      requestWorld
                      return NotExecutedGameCanceled
                    -- these values are never supposed to be used here:
                    New -> do
                      serverError "logic : newStatus == New"
                      return NotExecutedGameCanceled))

--  initializePlayers :: StateT (ServerState s) IO (Maybe (Time Duration System))
  initializePlayers = liftIO getSystemTime >>= \start -> do
    -- we add one second to take into account the fact that start of game is delayed by one second.
    adjustAll $ \c -> c { getShipSafeUntil = Just $ addDuration (fromSecs 6) start }
    return Nothing

--  stepWorld :: Time Point System -> StateT (ServerState s) IO (Maybe (Time Duration System))
  stepWorld now = do
    let !zero = zeroCoords
        mult = initalGameMultiplicator
    accs <- Map.mapMaybe
      (\p ->
          let !acc = getShipAcceleration $ unClient p
            in if acc == zero
              then Nothing
              else Just acc)
      <$> gets onlyPlayersMap
    updateSafeShips >>= \shipsLostArmor ->
      updateVoice >>= \noteChange ->
        notifyPlayersN (map (flip PlayMusic SineSynth) noteChange ++ [GameEvent $ PeriodicMotion accs shipsLostArmor])
    adjustAll $ \p -> p { getShipAcceleration = zeroCoords }
    return $ Just $ toSystemDuration mult gameMotionPeriod

   where

    updateVoice =
      gets unServerState >>= \s ->
        liftIO $ modifyMVar
          (scheduledGame s)
          (\g -> let (newScore, noteChange) = stepScore $ score g in return (g {score = newScore}, noteChange))

    updateSafeShips = adjustAll' $ \c@(HamazedClient _ _ mayTimeUnsafe _ _ _) ->
      maybe
        Nothing
        (\timeUnsafe ->
          if timeUnsafe < now
            then
              Just $ c { getShipSafeUntil = Nothing }
            else
              Nothing)
        mayTimeUnsafe


--------------------------------------------------------------------------------
-- functions used in 'createClient' --------------------------------------------
--------------------------------------------------------------------------------

makePlayerName :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
               => SuggestedPlayerName -> m PlayerName
makePlayerName (SuggestedPlayerName sn) = do
  let go mayI = do
        let proposal = PlayerName $ pack $ maybe sn ((++) sn . show) mayI
        checkNameAvailability proposal >>= either
          (\_ -> go $ Just $ maybe (2::Int) succ mayI)
          (\_ -> return proposal)
  go Nothing

checkNameAvailability :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
                      => PlayerName -> m (Either Text ())
checkNameAvailability name =
  any ((== name) . getName . unClient) <$> gets clientsMap >>= \case
    True  -> return $ Left "Name is already taken"
    False -> return $ Right ()

--------------------------------------------------------------------------------
-- functions used in 'afterClientWasAdded' -------------------------------------
--------------------------------------------------------------------------------

participateToWorldCreation :: (MonadIO m, MonadState (ServerState HamazedServerState) m, MonadReader ConstClient m)
                           => WorldId
                           -- ^ if this 'WorldId' doesn't match with the 'WorldId' of 'worldCreation',
                           -- nothing is done because it is obsolete (eventhough the server cancels obsolete requests,
                           -- this case could occur if the request was cancelled just after the non-result was
                           -- sent by the client).
                           -> m ()
participateToWorldCreation key = asks clientId >>= \origin ->
  gets' worldCreation >>= \wc@(WorldCreation st wid _ _) ->
    bool
      (serverLog $ pure $ colored ("Obsolete key " <> pack (show key)) blue)
      (case st of
        Created ->
          serverLog $ pure $ colored ("World " <> pack (show key) <> " is already created.") blue
        CreationAssigned assignees -> do
          let prevSize = Set.size assignees
              single = Set.singleton origin
              newAssignees = Set.union single assignees
              newSize = Set.size newAssignees
          unless (newSize == prevSize) $
            -- a previously assigned client has disconnected, reconnects and sends a non-result
            serverLog $ pure $ colored ("Adding assignee : " <> pack (show origin)) blue
          modify' $ mapState $ \s -> s { worldCreation = wc { creationState = CreationAssigned newAssignees
                                                 } }
          gets clientsMap >>= requestWorldBy . flip Map.restrictKeys single)
      $ wid == key


--------------------------------------------------------------------------------
-- functions used in 'handleClientEvent' ---------------------------------------
--------------------------------------------------------------------------------

handleEvent :: (MonadIO m, MonadState (ServerState HamazedServerState) m, MonadReader ConstClient m)
            => ClientEventT HamazedServerState -> m ()
handleEvent = \case
  RequestApproval cmd@(AssignName name) -> either
    (notifyClient . CommandError cmd)
    (\_ -> checkNameAvailability name >>= either
      (notifyClient . CommandError cmd)
      (\_ -> do
        adjustClient $ \c -> c { getName = name }
        acceptCmd cmd)
    ) $ checkName $ unpack $ unPlayerName name
  RequestApproval cmd@(AssignColor _) ->
    notifyClient $ CommandError cmd "use Do PutColorSchemeCenter instead"
  RequestApproval cmd@(Says _) ->
    acceptCmd cmd
  RequestApproval (Leaves _) ->
    asks clientId >>= disconnect (ClientShutdown $ Right ()) -- will do the corresponding 'notifyEveryone $ RunCommand'
  Do cmd -> do
   case cmd of
    Put (ColorSchemeCenter color) -> do
      modify' $ mapState $ \s -> s { centerColor = color }
      adjustAllWithKey $ \i c -> c { getColor = mkClientColorFromCenter i color }
      gets clientsMap >>=
        notifyEveryoneN .
          map (\(k, c) -> RunCommand k (AssignColor $ getColor $ unClient c)) .
          Map.assocs
    Put (WorldShape s) ->
      onChangeWorldParams $ changeWorldShape s
    Succ x -> onDelta 1    x
    Pred x -> onDelta (-1) x
   publish $ Done cmd

  Report (Get ColorSchemeCenterKey) ->
    gets' centerColor >>= notifyClient . Reporting . Put . ColorSchemeCenter
  Report (Get WorldShapeKey) ->
    worldShape <$> gets' worldParameters >>= notifyClient . Reporting . Put . WorldShape

  ExitedState Excluded -> gets' intent >>= \case
    IntentSetup -> do
      -- change client state to make it playable
      adjustClient $ \c -> c { getState = Just ReadyToPlay }
      publish Joins
      -- The number of players just changed, so we need a new world.
      requestWorld
      -- next step when client 'IsReady'
    IntentPlayGame _ -> do
      notifyClient $ EnterState Excluded
      publish WaitsToJoin
  ExitedState Setup -> gets' intent >>= \case
    IntentSetup -> do
      modify' $ mapState $ \s -> s { intent = IntentPlayGame Nothing }
      publish StartsGame
      notifyPlayers $ ExitState Setup
      -- we need a new world so that the game starts on a world
      -- that players didn't have time to visually analyze yet.
      requestWorld
    IntentPlayGame _ -> return ()
  ExitedState (PlayLevel _) ->
    return ()

  LevelEnded outcome -> do
    adjustClient $ \c -> c { getState = Just $ Playing $ Just outcome }
    gets' intent >>= \case
      IntentPlayGame Nothing ->
        modify' $ mapState $ \s -> s { intent = IntentPlayGame $ Just outcome }
      IntentPlayGame (Just candidate) -> -- replace this by using the outcome in client state?
        case candidate of
          Won -> when (outcome /= candidate) $
            gameError $ "inconsistent outcomes:" ++ show (candidate, outcome)
          Lost _ -> case outcome of
            Won ->  gameError $ "inconsistent outcomes:" ++ show (candidate, outcome)
            Lost _ -> return () -- we allow losing reasons to differ
      IntentSetup ->
        gameError "LevelEnded received while in IntentSetup"
    gets onlyPlayersMap >>= \players' -> do
      let playing Nothing = error "should not happen"
          playing (Just ReadyToPlay) = error "should not happen"
          playing (Just (Playing Nothing)) = True
          playing (Just (Playing (Just _))) = False
          (playersPlaying, playersDonePlaying) = Map.partition (playing . getState . unClient) players'
          gameStatus =
            if null playersPlaying
              then
                WhenAllPressedAKey (OutcomeValidated outcome) (Just 2) $ Map.map (const False) playersDonePlaying
              else
                WaitingForOthersToEndLevel $ Map.keysSet playersPlaying
      notifyN [EnterState $ PlayLevel gameStatus] playersDonePlaying
      gets' scheduledGame >>= \g -> liftIO (tryTakeMVar g) >>= maybe
        (warning "LevelEnded sent while game is Nothing")
        (\game -> void $ liftIO $ putMVar g $! game { status' = gameStatus })
  CanContinue next ->
    gets' scheduledGame >>= \g -> liftIO (tryReadMVar g) >>= maybe
      (warning "CanContinue sent while game is Nothing")
      (\game -> do
          let curStatus = status' game
          case curStatus of
            WhenAllPressedAKey x Nothing havePressed -> do
              when (x /= next) $ error $ "inconsistent:"  ++ show (x,next)
              i <- asks clientId
              let newHavePressed = Map.insert i True havePressed
                  intermediateStatus = WhenAllPressedAKey x Nothing newHavePressed
              liftIO $ void $ swapMVar g $! game { status' = intermediateStatus }
              -- update to avoid state where the map is equal to all players:
              void $ updateCurrentStatus $ Just curStatus
            _ -> error $ "inconsistent:"  ++ show curStatus)

  WorldProposal wid mkEssenceRes stats -> case mkEssenceRes of
    Impossible errs -> gets' intent >>= \case
      IntentSetup -> gets' levelSpecification >>= notifyPlayers . GameInfo . CannotCreateLevel errs . levelNumber
      IntentPlayGame _ ->
        fmap levelNumber (gets' levelSpecification) >>= \ln -> do
          notifyPlayers $ GameInfo $ CannotCreateLevel errs ln
          serverError $ "Could not create level " ++ show ln ++ ":" ++ unpack (Text.intercalate "\n" errs)

    NeedMoreTime -> addStats stats wid
    Success essence -> gets unServerState >>= \(HamazedServerState _ levelSpec _ (WorldCreation st key spec prevStats) _ _ _) -> bool
      (serverLog $ pure $ colored ("Dropped obsolete world " <> pack (show wid)) blue)
      (case st of
        Created ->
          serverLog $ pure $ colored ("Dropped already created world " <> pack (show wid)) blue
        CreationAssigned _ -> do
          -- This is the first valid world essence, so we can cancel the request
          cancelWorldRequest
          let !newStats = safeMerge mergeStats prevStats stats
          log $ colored (pack $ show newStats) white
          modify' $ mapState $ \s -> s { worldCreation = WorldCreation Created key spec newStats }
          notifyPlayers $ ChangeLevel (mkLevelEssence levelSpec) essence key)
      $ key == wid
  CurrentGameState wid mayGameStateEssence -> maybe
    (handlerError $ "Could not get GameState " ++ show wid)
    (\gameStateEssence ->
      gets' scheduledGame >>= liftIO . tryReadMVar >>= \case
        Just (CurrentGame _ gamePlayers (Paused _ _) _) -> do
          disconnectedPlayerKeys <- Set.difference gamePlayers . Map.keysSet <$> gets onlyPlayersMap
          flip Map.restrictKeys disconnectedPlayerKeys <$> gets clientsMap >>=
            notifyN [PutGameState gameStateEssence wid]
        invalid -> handlerError $ "CurrentGameState sent while game is " ++ show invalid)
    mayGameStateEssence

  IsReady wid -> do
    adjustClient $ \c -> c { getCurrentWorld = Just wid }
    gets' intent >>= \case
      IntentSetup ->
        -- (follow-up from 'ExitedState Excluded')
        -- Allow the client to setup the world, now that the world contains its ship.
        notifyClient $ EnterState Setup
      IntentPlayGame maybeOutcome ->
        gets unServerState >>= \(HamazedServerState _ _ _ (WorldCreation _ lastWId _ _) _ _ game) ->
         liftIO (tryReadMVar game) >>= maybe
          (do
            adjustClient $ \c -> c { getState = Just ReadyToPlay }
            -- start the game when all players have the right world
            players <- gets onlyPlayersMap
            let playersAllReady =
                  all ((== Just lastWId) . getCurrentWorld . unClient) players
            when playersAllReady $ do
              adjustAll $ \c ->
                case getState c of
                  Just ReadyToPlay ->
                    c { getState = Just $ Playing Nothing
                      , getShipAcceleration = zeroCoords }
                  _ -> c
              -- 'putMVar' is non blocking because all game changes are done inside a modifyMVar
              -- of 'ServerState' and we are inside one.
              void $ liftIO $ putMVar game $ mkCurrentGame lastWId $ Map.keysSet players)
          -- a game is in progress (reconnection scenario) :
          (\(CurrentGame wid' _ _ _) -> do
              when (wid' /= lastWId) $
                handlerError $ "reconnection failed " ++ show (wid', lastWId)
              -- make player join the current game, but do /not/ set getShipSafeUntil,
              -- else disconnecting / reconnecting intentionally could be a way to cheat
              -- by having more safe time.
              adjustClient $ \c -> c { getState = Just $ Playing maybeOutcome
                                     , getShipAcceleration = zeroCoords })

  -- Due to network latency, laser shots may be applied to a world state
  -- different from what the player saw when the shot was made.
  -- But since the laser shot will be rendered with latency, too, the player will be
  -- able to integrate the latency via this visual feedback - provided that latency is stable over time.
  Action Laser dir ->
    asks clientId >>= notifyPlayers . GameEvent . LaserShot dir
  Action Ship dir ->
    adjustClient $ \c -> c { getShipAcceleration = sumCoords (coordsForDirection dir) $ getShipAcceleration c }
 where
  publish a = asks clientId >>= notifyEveryone . PlayerInfo a
  acceptCmd cmd = asks clientId >>= notifyEveryone . flip RunCommand cmd

onDelta :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
        => Int
        -> SharedEnumerableValueKey
        -> m ()
onDelta i key = onChangeWorldParams $ \wp -> case key of
  BlockSize -> case wallDistrib wp of
    p@(WallDistribution prevSize _) ->
      let adjustedSize
           | newSize < minBlockSize = minBlockSize
           | newSize > maxBlockSize = maxBlockSize
           | otherwise = newSize
           where newSize = prevSize + i
      in bool
        (Just $ wp { wallDistrib = p { blockSize' = adjustedSize } })
        Nothing $
        adjustedSize == prevSize
  WallProbability -> case wallDistrib wp of
    p@(WallDistribution _ prevProba) ->
      let adjustedProba
           | newProba < minWallProba = minWallProba
           | newProba > maxWallProba = maxWallProba
           | otherwise = newProba
           where
             newProba = wallProbaIncrements * fromIntegral (round (newProba' / wallProbaIncrements) :: Int)
             newProba' = minWallProba + wallProbaIncrements * fromIntegral nIncrements
             nIncrements = i + round ((prevProba - minWallProba) / wallProbaIncrements)
      in bool
        (Just $ wp { wallDistrib = p { wallProbability' = adjustedProba } })
        Nothing $
        adjustedProba == prevProba

onChangeWorldParams :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
                    => (WorldParameters -> Maybe WorldParameters)
                    -> m ()
onChangeWorldParams f =
  state (\s ->
    let mayNewParams = f prevParams
        prevParams = worldParameters $ unServerState s
    in (mayNewParams
      , maybe id (\newParams -> mapState (\s' -> s' { worldParameters = newParams })) mayNewParams s))
    >>= maybe (return ()) onChange
 where
  onChange p = do
    notifyEveryone $ OnWorldParameters p
    requestWorld

addStats :: (MonadIO m, MonadState (ServerState HamazedServerState) m, MonadReader ConstClient m)
         => Map Properties Statistics
         -> WorldId
         -- ^ if this 'WorldId' doesn't match with the 'WorldId' of 'worldCreation',
         -- nothing is done because it is obsolete (eventhough the server cancels obsolete requests,
         -- this case could occur if the request was cancelled just after the non-result was
         -- sent by the client).
         -> m ()
addStats stats key =
  gets' worldCreation >>= \wc@(WorldCreation st wid _ prevStats) -> do
    let newStats = safeMerge mergeStats prevStats stats
    bool
      (serverLog $ pure $ colored ("Obsolete key " <> pack (show key)) blue)
      (case st of
          -- drop newStats if world is already created.
        Created -> return ()
        CreationAssigned _ -> do
          log $ colored (pack $ show newStats) white
          modify' $ mapState $ \s -> s { worldCreation = wc { creationStatistics = newStats } })
      $ wid == key

gameError :: (MonadIO m, MonadState (ServerState HamazedServerState) m, MonadReader ConstClient m)
          => String -> m ()
gameError = error' "Game"

--------------------------------------------------------------------------------
-- functions used in 'afterClientLeft' -----------------------------------------
--------------------------------------------------------------------------------

requestWorld :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
             => m ()
requestWorld = do
  cancelWorldRequest
  modify' $ (\s ->
    let (HamazedServerState _ level params creation _ _ _) = unServerState s
        nextWid = succ $ creationKey creation
    in mapState
      (\v -> v {
        worldCreation = WorldCreation
          (CreationAssigned $ Map.keysSet $ clientsMap s)
          nextWid
          (WorldSpec level (Map.keysSet $ onlyPlayersMap s) params)
          Map.empty
              })
      s )
  gets clientsMap >>= requestWorldBy

requestWorldBy :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
               => Map ClientId (Client HamazedClient) -> m ()
requestWorldBy x =
  gets' worldCreation >>= \(WorldCreation _ wid spec _) ->
    notifyN [WorldRequest wid $ Build (fromSecs 1) spec] x

cancelWorldRequest :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
                   => m ()
cancelWorldRequest = gets' worldCreation >>= \wc@(WorldCreation st wid _ _) -> case st of
  Created -> return ()
  CreationAssigned assignees -> unless (Set.null assignees) $ do
    gets clientsMap >>= notifyN [WorldRequest wid Cancel] . flip Map.restrictKeys assignees
    modify' $ mapState (\s -> s { worldCreation = wc { creationState = CreationAssigned Set.empty } })


gets' :: (MonadState (ServerState HamazedServerState) m)
      => (HamazedServerState -> a) -> m a
gets' f = gets (f . unServerState)

unAssign :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
         => ClientId
         -> m ()
unAssign sid = gets' worldCreation >>= \wc -> case creationState wc of
  Created -> return ()
  CreationAssigned assignees -> do
    let s1 = Set.size assignees
        newAssignees = Set.delete sid assignees
        s2 = Set.size newAssignees
    unless (s1 == s2) $
      modify' $ mapState (\s -> s { worldCreation = wc { creationState = CreationAssigned newAssignees } })

onlyPlayersMap :: ServerState HamazedServerState
               -> Map ClientId (Client HamazedClient)
onlyPlayersMap = Map.filter (isJust . getState . unClient) . clientsMap

--------------------------------------------------------------------------------
-- functions used in multiple --------------------------------------------------
--------------------------------------------------------------------------------

{-# INLINABLE notifyPlayersN #-}
notifyPlayersN :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
               => [ServerEventT HamazedServerState] -> m ()
notifyPlayersN evts =
  notifyN evts =<< gets onlyPlayersMap

{-# INLINABLE notifyPlayers #-}
notifyPlayers :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
              => ServerEventT HamazedServerState -> m ()
notifyPlayers evt =
  notifyN [evt] =<< gets onlyPlayersMap
