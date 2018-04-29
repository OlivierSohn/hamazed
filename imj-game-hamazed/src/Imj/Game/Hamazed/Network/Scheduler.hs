{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Network.Scheduler
      ( updateCurrentStatus
      , gameScheduler
      ) where

import           Imj.Prelude

import           Control.Concurrent(threadDelay)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.State.Strict(MonadState, runStateT, modify', get, gets)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text(pack)
import           Data.Tuple(swap)
import           UnliftIO.MVar (MVar
                                , modifyMVar, swapMVar
                                , readMVar, tryReadMVar, tryTakeMVar, putMVar)

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Color.Types
import           Imj.Server.Internal.Types

import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Network.Notify
import           Imj.Graphics.Text.ColorString(colored)
import           Imj.Music
import           Imj.Server
import           Imj.Server.Log
import           Imj.Server.Connection

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

data RunResult =
    NotExecutedGameCanceled
  | NotExecutedTryAgainLater !(Time Duration System)
  -- ^ withe the duration to sleep before retrying
  | Executed !(Maybe (Time Duration System))
  -- ^ With an optional duration to wait before the next iteration

-- Game Timing:
{-
{-# INLINABLE onMove #-}
onMove :: (MonadState AppState m, MonadIO m)
       => Map ClientId (Coords Vel)
       -> [ClientId]
       -> m ()
onMove accelerations shipsLosingArmor = getGameState >>= \(GameState _ m@(Multiplicator mv) world a b c d e) ->
  liftIO getSystemTime >>= \t -> do
    let nextTime = addDuration (toSystemDuration m gameMotionPeriod) t
    putGameState $ GameState (Just nextTime) (Multiplicator (mv + 0.01)) (moveWorld accelerations shipsLosingArmor world) a b c d e
    onHasMoved t
-}
