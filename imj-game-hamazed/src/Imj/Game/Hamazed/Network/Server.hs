{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
This module exports types related to networking for Hamazed game.

Game events are sent by the clients, proccessed by the server. For example, if two players
play the game:

@
  - Ax = acceleration of ship x
  - Lx = laser shot of ship x
  - .  = end of a game period

        >>> time >>>
 . . . A1 . . A1 A2 L2 L1 .
              ^^^^^ ^^^^^
              |     |
              |     laser shots can't be aggregated.
              |
              accelerations can be aggregated, their order within a period is unimportant.
@

The order in which L1 L2 are handled by the server is the order in which they are received.
This is /unfair/ because one player (due to network delays) could have rendered the
last period 100ms before the other, thus having a significant advantage over the other player.
We could be more fair by keeping track of the /perceived/ time on the player side:

when sending a game action event, we could send along the difference between the system time of the action
and the system time at which the last motion update was presented to the player.

Hence, to know how to order close laser shots, if the ships are on the same row or column,
the server should wait a little (max. 50 ms?) to see if the other player makes a
perceptually earlier shot.
-}

module Imj.Game.Hamazed.Network.Server
      ( HamazedServer(..)
      ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay)
import           Control.Concurrent.MVar.Strict(MVar)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Reader(asks)
import           Control.Monad.State.Strict(MonadState, modify', gets, get, state, runStateT)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe(isJust)
import qualified Data.Set as Set
import           Data.Text(pack, unpack, unlines)
import qualified Data.Text as Text(intercalate)
import           Data.Tuple(swap)
import           UnliftIO.MVar (modifyMVar, swapMVar, readMVar, tryReadMVar, tryTakeMVar, putMVar, newEmptyMVar)

import           Imj.ClientView.Types
import           Imj.Game.Hamazed.World.Types
import           Imj.Game.Hamazed.HighScores
import           Imj.Game.Hamazed.Level
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Setup
import           Imj.Game.Hamazed.Network.State
import           Imj.Graphics.Color.Types
import           Imj.Server.Class
import           Imj.Server.Types
import           Imj.Space.Types

import           Imj.Game.Hamazed.Timing
import           Imj.Game.Level
import           Imj.Game.Status
import           Imj.Graphics.Text.ColorString(colored, intercalate)
import           Imj.Music.Types hiding(Do)
import           Imj.Music.Play
import           Imj.Server.Connection
import           Imj.Server.Log
import           Imj.Server.Run
import           Imj.Server
import           Imj.Timing


-- | 'Hamazed' handles a single game.
data HamazedServer = HamazedServer {
    gameTiming :: !GameTiming -- could / should this be part of CurrentGame?
  , levelSpecification :: {-# UNPACK #-} !LevelSpec
  -- ^ The actual 'World' is stored on the clients
  , worldCreation :: {-unpack sum-} !WorldCreation
  , intent :: {-unpack sum-} !Intent
  -- ^ Influences the control flow (how 'ClientEvent's are handled).
  , highScores :: !HighScores
  , scheduledGame :: {-# UNPACK #-} !(MVar CurrentGame)
  -- ^ When set, it informs the scheduler thread that it should run the game.
} deriving(Generic)
instance NFData HamazedServer
instance ServerInit HamazedServer where
  type ClientViewT HamazedServer = HamazedClient

  mkInitialState =
    (,) params . HamazedServer mkGameTiming lvSpec wc IntentSetup mkEmptyHighScores <$> newEmptyMVar
   where
    lvSpec = LevelSpec firstServerLevel CannotOvershoot
    params = initialParameters
    wc = mkWorldCreation $ WorldSpec lvSpec Set.empty params

  mkInitialClient = HamazedClient Nothing Nothing zeroCoords Nothing

instance ServerCmdParser HamazedServer

instance ServerInParallel HamazedServer where

  inParallel = [gameScheduler]

instance ServerClientHandler HamazedServer where

  type StateValueT   HamazedServer = GameStateValue

  type ClientEventT HamazedServer = HamazedClientEvent

  handleClientEvent = \case
    LevelEnded outcome -> do
      adjustClient_ $ \c -> c { getState = Just $ Playing $ Just outcome }
      getsState intent >>= \case
        IntentPlayGame Nothing ->
          modifyState $ \s -> s { intent = IntentPlayGame $ Just outcome }
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
            (playersPlaying, playersDonePlaying) = Map.partition (playing . getState . unClientView) players'
            gameStatus =
              if null playersPlaying
                then
                  WhenAllPressedAKey (OutcomeValidated outcome) (Just 2) $ Map.map (const False) playersDonePlaying
                else
                  WaitingForOthersToEndLevel $ Map.keysSet playersPlaying
        notifyN' [EnterState $ Included $ PlayLevel gameStatus] playersDonePlaying
        getsState scheduledGame >>= \g -> liftIO (tryTakeMVar g) >>= maybe
          (warning "LevelEnded sent while game is Nothing")
          (\game -> void $ liftIO $ putMVar g $! game { status' = gameStatus })
        return []
    CanContinue next -> do
      getsState scheduledGame >>= \g -> liftIO (tryReadMVar g) >>= maybe
        (warning "CanContinue sent while game is Nothing")
        (\game -> do
            let curStatus = status' game
            case curStatus of
              WhenAllPressedAKey x Nothing havePressed -> do
                when (x /= next) $ error $ "inconsistent:"  ++ show (x,next)
                i <- asks clientId
                let intermediateStatus = WhenAllPressedAKey x Nothing $ Map.insert i True havePressed
                liftIO $ void $ swapMVar g $! game { status' = intermediateStatus }
                -- update to avoid state where the map is equal to all players:
                void $ updateCurrentStatus $ Just curStatus
              _ -> error $ "inconsistent:"  ++ show curStatus)
      return []
    WorldProposal wid mkEssenceRes stats -> do
     case mkEssenceRes of
      Impossible errs -> getsState intent >>= \case
        IntentSetup -> getsState levelSpecification >>= notifyPlayers . GameInfo . CannotCreateLevel errs . levelNumber
        IntentPlayGame _ ->
          fmap levelNumber (getsState levelSpecification) >>= \ln -> do
            notifyPlayers $ GameInfo $ CannotCreateLevel errs ln
            serverError $ "Could not create level " ++ show ln ++ ":" ++ unpack (Text.intercalate "\n" errs)

      NeedMoreTime -> addStats stats wid
      Success essence -> gets unServerState >>= \(HamazedServer _ levelSpec (WorldCreation st key spec prevStats) _ _ _) -> bool
        (serverLog $ pure $ colored ("Dropped obsolete world " <> pack (show wid)) blue)
        (case st of
          Created ->
            serverLog $ pure $ colored ("Dropped already created world " <> pack (show wid)) blue
          CreationAssigned _ -> do
            -- This is the first valid world essence, so we can cancel the request
            cancelWorldRequest
            let !newStats = safeMerge mergeStats prevStats stats
            log $ colored (pack $ show newStats) white
            modifyState $ \s -> s { worldCreation = WorldCreation Created key spec newStats }
            notifyPlayers $ ChangeLevel (mkLevelEssence levelSpec) essence key)
        $ key == wid
     return []
    CurrentGameState wid mayGameStateEssence -> do
     maybe
      (handlerError $ "Could not get HamazedGame " ++ show wid)
      (\gameStateEssence ->
        getsState scheduledGame >>= liftIO . tryReadMVar >>= \case
          Just (CurrentGame _ gamePlayers (Paused _ _) _) -> do
            disconnectedPlayerKeys <- Set.difference gamePlayers . Map.keysSet <$> gets onlyPlayersMap
            flip Map.restrictKeys disconnectedPlayerKeys <$> gets clientsMap >>=
              notifyN [PutGameState gameStateEssence wid]
          invalid -> handlerError $ "CurrentGameState sent while game is " ++ show invalid)
      mayGameStateEssence
     return []
    IsReady wid -> do
      adjustClient_ $ \c -> c { getCurrentWorld = Just wid }
      getsState intent >>= \case
        IntentSetup ->
          -- Allow the client to setup the world, now that the world contains its ship.
          notifyClient' $ EnterState $ Included Setup
        IntentPlayGame maybeOutcome ->
          gets unServerState >>= \(HamazedServer _ lev (WorldCreation _ lastWId _ _) _ _ game) ->
           liftIO (tryReadMVar game) >>= maybe
            (do
              adjustClient_ $ \c -> c { getState = Just ReadyToPlay }
              -- start the game when all players have the right world
              players <- gets onlyPlayersMap
              let playersAllReady =
                    all ((== Just lastWId) . getCurrentWorld . unClientView) players
              when playersAllReady $ do
                adjustAllClients $ \c ->
                  case getState c of
                    Just ReadyToPlay ->
                      c { getState = Just $ Playing Nothing
                        , getShipAcceleration = zeroCoords }
                    _ -> c
                -- 'putMVar' is non blocking because all game changes are done inside a modifyMVar
                -- of 'ServerState' and we are inside one.
                void $ liftIO $ putMVar game $ mkCurrentGame (scoreForLevel lev) lastWId $ Map.keysSet players)
            -- a game is in progress (reconnection scenario) :
            (\(CurrentGame wid' _ _ _) -> do
                when (wid' /= lastWId) $
                  handlerError $ "reconnection failed " ++ show (wid', lastWId)
                -- make player join the current game, but do /not/ set getShipSafeUntil,
                -- else disconnecting / reconnecting intentionally could be a way to cheat
                -- by having more safe time.
                adjustClient_ $ \c -> c { getState = Just $ Playing maybeOutcome
                                        , getShipAcceleration = zeroCoords })
      return []

    -- Due to network latency, laser shots may be applied to a world state
    -- different from what the player saw when the shot was made.
    -- But since the laser shot will be rendered with latency, too, the player will be
    -- able to integrate the latency via this visual feedback - provided that latency is stable over time.
    Action Laser dir -> do
      asks clientId >>= notifyPlayers . GameEvent . LaserShot dir
      return []
    Action Ship dir -> do
      adjustClient_ $ \c -> c { getShipAcceleration = sumCoords (coordsForDirection dir) $ getShipAcceleration c }
      return []

  type ValueT        HamazedServer = HamazedValue
  type ValueKeyT     HamazedServer = HamazedValueKey
  type EnumValueKeyT HamazedServer = HamazedEnumValueKey
  type ValuesT       HamazedServer = WorldParameters

  getValue WorldShapeKey =
    WorldShape . worldShape

  onPut (WorldShape s) =
    onChangeWorldParams $ changeWorldShape s

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

instance ServerClientLifecycle HamazedServer where

  onStartClient = \case
    NewClient -> return ()
    ReconnectingClient ->
      getsState scheduledGame >>= liftIO . tryReadMVar >>= maybe
        (serverLog $ pure "No game is running")
        (\(CurrentGame _ gamePlayers status _) -> case status of
            (Paused _ _) -> do
            -- we /could/ use 'Paused' ignored argument but it's probably out-of-date
            -- - the game scheduler thread updates it every second only -
            -- so we recompute the difference:
              connectedPlayers <- gets onlyPlayersMap
              let connectedPlayerKeys = Map.keysSet connectedPlayers
                  disconnected = Set.difference gamePlayers connectedPlayerKeys
                  gameConnectedPlayers = Map.keysSet $ Map.restrictKeys connectedPlayers gamePlayers
              bool
                (do serverLog $ (\i -> "A paused game exists, " <> intercalate " " i <> " are disconnected.")
                      <$> mapM showId (Set.toList disconnected)
                    asks clientId >>= bool
                      (return ())
                      (getsState worldCreation >>= \(WorldCreation creationSt wid _ _) -> case creationSt of
                        Created ->
                          -- The game is paused, and a world was created:
                          -- we ask one of the connected players to broadcast the current world
                          -- so that the newcomer can get it.
                          gets clientsMap >>= notifyN [WorldRequest wid GetGameState] . Map.take 1 . flip Map.restrictKeys gameConnectedPlayers
                        CreationAssigned _ ->
                          -- The game is paused, and a "next level" is being built:
                          -- add the newcomer to the assigned builders.
                          participateToWorldCreation wid)
                      . flip Set.member disconnected)
                (serverLog $ pure "A paused game exists but all players are connected.")
                $ Set.null disconnected
            _ -> serverLog $ pure "A game is in progress.")


  clientCanJoin _ = getsState intent >>= \case
    IntentSetup -> do
      -- change client state to make it playable
      adjustClient_ $ \c -> c { getState = Just ReadyToPlay }
      -- The number of players just changed, so we need a new world.
      requestWorld
      return True
    IntentPlayGame _ ->
      return False
  clientCanTransition = \case
    Setup -> getsState intent >>= \case
      IntentSetup -> do
        modifyState $ \s -> s { intent = IntentPlayGame Nothing }
        notifyPlayersN' [ExitState $ Included Setup]
        -- we need a new world so that the game starts on a world
        -- that players didn't have time to visually analyze yet.
        requestWorld
        return True
      IntentPlayGame _ ->
        return False
    PlayLevel _ ->
      return False

  afterClientLeft cid = do
    unAssign cid
    getsState intent >>= \case
      IntentSetup -> requestWorld -- because the number of players has changed
      IntentPlayGame _ -> return () -- don't create a new world while a game is in progress!

instance Server HamazedServer where

  type ServerEventT HamazedServer = HamazedServerEvent

--------------------------------------------------------------------------------
-- functions used in 'inParallel' ----------------------------------------------
--------------------------------------------------------------------------------

onLevelOutcome :: (MonadIO m, MonadState (ServerState HamazedServer) m)
               => Set ClientId -> LevelOutcome -> m ()
onLevelOutcome playerIds outcome =
  levelNumber <$> getsState levelSpecification >>= \levelN -> do
    -- by constructions, there will be no collision on 'ClientName Approved'
    players <- Set.fromList . map getName . Map.elems . flip Map.restrictKeys playerIds <$> gets clientsMap
    hs <- case outcome of
      Lost{} ->
        fmap Just <$> modifyState' $ \s ->
          let newScores = insertScore (pred levelN) players $ highScores s
          in (newScores, s { highScores = newScores })
      Won -> if levelN == lastLevel
        then
          fmap Just <$> modifyState' $ \s ->
            let newScores = insertScore levelN players $ highScores s
            in (newScores, s { highScores = newScores })
        else
          return Nothing

    notifyEveryoneN $
      (GameInfo $ LevelResult levelN outcome):
      [GameInfo GameWon | outcome == Won && levelN == lastLevel] ++
      map (GameInfo . Highscores) (maybeToList hs)

    adjustAllClients (\c -> case getState c of
      Just (Playing _) -> c { getState = Just ReadyToPlay }
      _ -> c)

    modifyState $ \s ->
     if outcome == Won && levelN < lastLevel
      then -- next level
        s { levelSpecification = (levelSpecification s) { levelNumber = succ levelN }
          , intent = IntentPlayGame Nothing
          }
      else -- end game
        s { levelSpecification = (levelSpecification s) { levelNumber = firstServerLevel }
          , intent = IntentSetup
          }
    getsState intent >>= \case
      IntentSetup ->
        -- make fresh clients become players
        adjustAllClients' (\c -> case getState c of
          Just (Playing _) -> error "inconsistent"
          Just ReadyToPlay -> Nothing
          Nothing -> Just $ c { getState = Just ReadyToPlay })
          >>= notifyEveryoneN' . map (PlayerInfo Joins) . Set.toList
      IntentPlayGame _ -> return ()

-- | To avoid deadlocks, this function should be called only from inside a
-- transaction on ServerState.
{-# INLINABLE updateCurrentStatus #-}
updateCurrentStatus :: (MonadState (ServerState HamazedServer) m, MonadIO m)
                    => Maybe GameStatus
                    -- ^ the reference to take into account, if different from current status.
                    -> m (Maybe GameStatus)
updateCurrentStatus ref = getsState scheduledGame >>= tryReadMVar >>= maybe
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
onChangeStatus :: (MonadState (ServerState HamazedServer) m, MonadIO m)
               => Map ClientId (ClientView HamazedClient)
               -> GameStatus
               -> GameStatus
               -> m ()
onChangeStatus notified status newStatus = getsState scheduledGame >>= \game -> tryReadMVar game >>= maybe
  (return ())
  (\g -> do
    -- mayWorld can be concurrently modified from client handler threads, but only from
    -- inside a 'modifyMVar' on 'ServerState'. Since we are ourselves inside
    -- a 'modifyMVar' of 'ServerState', we guarantee that the 'takeMVar' inside
    -- 'swapMVar' won't block, because in the same 'modifyMVar' transaction, 'tryReadMVar'
    -- returned a 'Just'.
    liftIO $ void $ swapMVar game $! g {status' = newStatus}
    serverLog $ pure $
      colored (unlines
        [ "Game status change"
        , "from: " <> pack (show status)
        , "to  : " <> pack (show newStatus)
        ]) yellow
    notifyN' [EnterState $ Included $ PlayLevel newStatus] notified)

gameScheduler :: MVar (ServerState HamazedServer) -> IO ()
gameScheduler st =
  readMVar st >>= \(ServerState _ _ terminate _ _ (HamazedServer _ _ _ _ _ mayGame)) ->
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
    stopMusic = gets unServerState >>= \(HamazedServer _ _ _ _ _ game) ->
      tryTakeMVar game >>= maybe
        (return ())
        (\g@(CurrentGame _ _ _ s) -> do
          let (newScore, notesChanges) = stopScore s
          case notesChanges of
            [] -> return ()
            _:_ -> notifyPlayersN' $ map (flip PlayMusic SineSynth) notesChanges
          putMVar game $ g{score = newScore})

    go = get >>= \(ServerState _ _ terminate _ _ (HamazedServer _ _ _ _ _ game)) ->
      if terminate
        then do
          serverLog $ pure $ colored "Terminating game" yellow
          return NotExecutedGameCanceled
        else
          -- we use 'tryReadMVar' to /not/ block here, as we are inside a modifyMVar.
          liftIO (tryReadMVar game) >>= maybe
            (do serverError "logic : mayGame is Nothing"
                return NotExecutedGameCanceled)
            (\(CurrentGame curWid gamePlayers _ _) ->
              if refWid /= curWid
                then do
                  serverLog $ pure $ colored ("The world has changed: " <> pack (show (curWid, refWid))) yellow
                  return NotExecutedGameCanceled -- the world has changed
                else
                  updateCurrentStatus Nothing >>= maybe (return NotExecutedGameCanceled) (\case
                    Paused _ _ ->
                      return $ NotExecutedTryAgainLater $ fromSecs 1
                    CancelledNoConnectedPlayer -> do
                      onLevelOutcome gamePlayers $ Lost "All players left"
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
                      onLevelOutcome gamePlayers outcome
                      requestWorld
                      return NotExecutedGameCanceled
                    -- these values are never supposed to be used here:
                    New -> do
                      serverError "logic : newStatus == New"
                      return NotExecutedGameCanceled))

--  initializePlayers :: StateT (ServerState s) IO (Maybe (Time Duration System))
  initializePlayers = liftIO getSystemTime >>= \start -> do
    -- we add one second to take into account the fact that start of game is delayed by one second.
    adjustAllClients $ \c -> c { getShipSafeUntil = Just $ addDuration (fromSecs 6) start }
    return Nothing

--  stepWorld :: Time Point System -> StateT (ServerState s) IO (Maybe (Time Duration System))
  stepWorld now = do
    let !zero = zeroCoords
        mult = initalGameMultiplicator
    accs <- Map.mapMaybe
      (\p ->
          let !acc = getShipAcceleration $ unClientView p
            in if acc == zero
              then Nothing
              else Just acc)
      <$> gets onlyPlayersMap
    updateSafeShips >>= \shipsLostArmor ->
      updateVoice >>= \noteChange ->
        notifyPlayersN'
          (map (flip PlayMusic SineSynth) noteChange ++
          [ServerAppEvt $ GameEvent $ PeriodicMotion accs shipsLostArmor])
    adjustAllClients $ \p -> p { getShipAcceleration = zeroCoords }
    return $ Just $ toSystemDuration mult gameMotionPeriod

   where

    updateVoice =
      gets unServerState >>= \s ->
        liftIO $ modifyMVar
          (scheduledGame s)
          (\g -> let (newScore, noteChange) = stepScore $ score g in return (g {score = newScore}, noteChange))

    updateSafeShips = adjustAllClients' $ \c@(HamazedClient _ mayTimeUnsafe _ _) ->
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
-- functions used in 'afterClientWasAdded' -------------------------------------
--------------------------------------------------------------------------------

participateToWorldCreation :: (MonadIO m, MonadState (ServerState HamazedServer) m, MonadReader ConstClientView m)
                           => WorldId
                           -- ^ if this 'WorldId' doesn't match with the 'WorldId' of 'worldCreation',
                           -- nothing is done because it is obsolete (eventhough the server cancels obsolete requests,
                           -- this case could occur if the request was cancelled just after the non-result was
                           -- sent by the client).
                           -> m ()
participateToWorldCreation key = asks clientId >>= \origin ->
  getsState worldCreation >>= \wc@(WorldCreation st wid _ _) ->
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
          modifyState $ \s -> s { worldCreation = wc { creationState = CreationAssigned newAssignees
                                                 } }
          gets clientsMap >>= requestWorldBy . flip Map.restrictKeys single)
      $ wid == key


--------------------------------------------------------------------------------
-- functions used in 'handleClientEvent' ---------------------------------------
--------------------------------------------------------------------------------

onChangeWorldParams :: (MonadIO m, MonadState (ServerState HamazedServer) m) -- TODO make more generic
                    => (WorldParameters -> Maybe WorldParameters)
                    -> m ()
onChangeWorldParams f =
  state (\s ->
    let mayNewParams = f prevParams
        prevParams = content s
    in (mayNewParams
      , maybe s (\newParams -> s { content = newParams }) mayNewParams))
    >>= maybe (return ()) onChange
 where
  onChange p = do
    notifyEveryone' $ OnContent p
    requestWorld

addStats :: (MonadIO m, MonadState (ServerState HamazedServer) m, MonadReader ConstClientView m)
         => Map Properties Statistics
         -> WorldId
         -- ^ if this 'WorldId' doesn't match with the 'WorldId' of 'worldCreation',
         -- nothing is done because it is obsolete (eventhough the server cancels obsolete requests,
         -- this case could occur if the request was cancelled just after the non-result was
         -- sent by the client).
         -> m ()
addStats stats key =
  getsState worldCreation >>= \wc@(WorldCreation st wid _ prevStats) -> do
    let newStats = safeMerge mergeStats prevStats stats
    bool
      (serverLog $ pure $ colored ("Obsolete key " <> pack (show key)) blue)
      (case st of
          -- drop newStats if world is already created.
        Created -> return ()
        CreationAssigned _ -> do
          log $ colored (pack $ show newStats) white
          modifyState $ \s -> s { worldCreation = wc { creationStatistics = newStats } })
      $ wid == key

gameError :: (MonadIO m, MonadState (ServerState HamazedServer) m, MonadReader ConstClientView m)
          => String -> m ()
gameError = error' "Game"

--------------------------------------------------------------------------------
-- functions used in 'afterClientLeft' -----------------------------------------
--------------------------------------------------------------------------------

requestWorld :: (MonadIO m, MonadState (ServerState HamazedServer) m)
             => m ()
requestWorld = do
  cancelWorldRequest
  modify' $ \s@(ServerState _ _ _ params _ (HamazedServer _ level creation _ _ _)) ->
    let nextWid = succ $ creationKey creation
    in mapState
      (\v -> v {
        worldCreation = WorldCreation
          (CreationAssigned $ Map.keysSet $ clientsMap s)
          nextWid
          (WorldSpec level (Map.keysSet $ onlyPlayersMap s) params)
          Map.empty
              })
      s
  gets clientsMap >>= requestWorldBy

requestWorldBy :: (MonadIO m, MonadState (ServerState HamazedServer) m)
               => Map ClientId (ClientView HamazedClient) -> m ()
requestWorldBy x =
  getsState worldCreation >>= \(WorldCreation _ wid spec _) ->
    notifyN [WorldRequest wid $ Build (fromSecs 1) spec] x

cancelWorldRequest :: (MonadIO m, MonadState (ServerState HamazedServer) m)
                   => m ()
cancelWorldRequest = getsState worldCreation >>= \wc@(WorldCreation st wid _ _) -> case st of
  Created -> return ()
  CreationAssigned assignees -> unless (Set.null assignees) $ do
    gets clientsMap >>= notifyN [WorldRequest wid Cancel] . flip Map.restrictKeys assignees
    modifyState (\s -> s { worldCreation = wc { creationState = CreationAssigned Set.empty } })

unAssign :: (MonadIO m, MonadState (ServerState HamazedServer) m)
         => ClientId
         -> m ()
unAssign sid = getsState worldCreation >>= \wc -> case creationState wc of
  Created -> return ()
  CreationAssigned assignees -> do
    let s1 = Set.size assignees
        newAssignees = Set.delete sid assignees
        s2 = Set.size newAssignees
    unless (s1 == s2) $
      modifyState (\s -> s { worldCreation = wc { creationState = CreationAssigned newAssignees } })

onlyPlayersMap :: ServerState HamazedServer
               -> Map ClientId (ClientView HamazedClient)
onlyPlayersMap = Map.filter (isJust . getState . unClientView) . clientsMap

--------------------------------------------------------------------------------
-- functions used in multiple functions ----------------------------------------
--------------------------------------------------------------------------------

{-# INLINABLE notifyPlayersN' #-}
notifyPlayersN' :: (MonadIO m, MonadState (ServerState HamazedServer) m)
               => [ServerEvent HamazedServer] -> m ()
notifyPlayersN' evts =
  notifyN' evts =<< gets onlyPlayersMap

{-# INLINABLE notifyPlayers #-}
notifyPlayers :: (MonadIO m, MonadState (ServerState HamazedServer) m)
              => ServerEventT HamazedServer -> m ()
notifyPlayers evt =
  notifyN [evt] =<< gets onlyPlayersMap
