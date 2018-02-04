{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Update
      ( updateAppState
      ) where

import           Imj.Prelude

import           Data.Maybe(isNothing)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World
import           Imj.Game.Hamazed.World.Ship
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.UI.RectContainer
import           Imj.Util

{-# INLINABLE updateAppState #-}
updateAppState :: (MonadState AppState m, MonadReader e m, Draw e, MonadIO m)
               => UpdateEvent
               -- ^ The 'Event' that should be handled here.
               -> m ()
updateAppState (Right evt) = case evt of
  Configuration char   -> updateGameParamsFromChar char
  StartGame            -> putUserIntent Play >> updateAppState (Right $ StartLevel firstLevel)
  EndGame              -> onEndGame
  StartLevel nextLevel -> onStartLevel nextLevel
  CycleRenderingOptions -> changeFont
  (Timeout (Deadline t _ AnimateUI)) -> updateUIAnim t
  (Timeout (Deadline _ _ (AnimateParticleSystem key))) -> liftIO getSystemTime >>= updateOneParticleSystem key
  (Timeout (Deadline _ _ DisplayContinueMessage)) -> onContinueMessage
  e -> error $ "The caller should handle :" ++ show e
updateAppState (Left evt) = case evt of
  ServerEvent (PeriodicMotion accelerations) -> onMove accelerations
  ServerEvent (LaserShot shipId dir) -> onLaser shipId dir

onStartLevel :: (MonadState AppState m, MonadIO m)
             => Int -> m ()
onStartLevel n =
  getGame >>= \(Game _ params state@(GameState _ _ (World _ ships _ _) _ _ _ _ (Screen sz _))) ->
    mkInitialState params sz n (map getShipId ships) (Just state) >>= putGameState

onEndGame :: (MonadState AppState m, Canvas e, MonadReader e m, MonadIO m) => m ()
onEndGame =
  getGameParameters >>= \params -> do
    getWorld >>= \(World _ ships _ _) -> do
      getTargetSize
        >>= liftIO . initialGameState params (map getShipId ships)
        >>= putGame . Game Configure params
      return ()

updateGameParamsFromChar :: (MonadState AppState m, MonadIO m)
                         => Char
                         -> m ()
updateGameParamsFromChar char =
 getGameParameters >>= \(GameParameters shape wallType mode) -> case char of
  '1' -> go Square wallType mode
  '2' -> go Rectangle2x1 wallType mode
  'e' -> go shape None mode
  'r' -> go shape Deterministic mode
  't' -> go shape (Random $ RandomParameters minRandomBlockSize StrictlyOneComponent) mode
  'd' -> go shape wallType CenterSpace
  'f' -> getShipId' >>= go shape wallType . CenterShip
  _ -> return ()
 where
  go a b c = putGameParameters (GameParameters a b c) >> onStartLevel 1
  getShipId' = undefined

onContinueMessage :: (MonadState AppState m)
                  => m ()
onContinueMessage =
  getGameState >>= \(GameState a t b c d (Level n target mayFinished) e f) -> do
    case mayFinished of
      Just (LevelFinished stop finishTime _) -> do
        let newLevel = Level n target (Just $ LevelFinished stop finishTime ContinueMessage)
        putGameState $ GameState a t b c d newLevel e f
      Nothing -> return ()

startGameState :: (MonadState AppState m)
               => Time Point System
               -> m ()
startGameState t =
  getGameState >>= \(GameState _ m world world' b d e f) ->
    putGameState
      $ GameState (Just t) m (startWorld t world) (startWorld t world') b d e f


onLaser :: (MonadState AppState m, MonadIO m)
        => ShipId
        -> Direction
        -> m ()
onLaser ship dir =
  liftIO getSystemTime >>= \t ->
    laserEventAction ship dir t >>= onDestroyedNumbers t

{-# INLINABLE onDestroyedNumbers #-}
onDestroyedNumbers :: (MonadState AppState m)
                   => Time Point System
                   -> [Number]
                   -> m ()
onDestroyedNumbers t destroyedBalls =
  getGameState >>= \(GameState b m world@(World _ ships (Space _ sz _) _)
                               futureWorld g level@(Level i target finished)
                   (UIAnimation (UIEvolutions j upDown _) k l) s) -> do
    (Screen _ center) <- getCurScreen
    mode <- getMode
    let destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
        allShotNumbers = g ++ destroyedNumbers
        ammos = map getAmmo ships
        allAmmos = sum ammos
        newLeft =
          let frameSpace = mkRectContainerWithCenterAndInnerSize center sz
              (horizontalDist, verticalDist) = computeViewDistances mode
              (_, _, leftMiddle, _) = getSideCenters $ mkRectContainerAtDistance frameSpace horizontalDist verticalDist
              infos = mkLeftInfo Normal ammos allShotNumbers level
          in mkTextAnimRightAligned leftMiddle leftMiddle infos 1 (fromSecs 0) -- 0 duration, since animation is over anyway
        newMultiplicator
          | null destroyedBalls = m
          | otherwise = initalGameMultiplicator
        newFinished = finished <|> checkTargetAndAmmo allAmmos (sum allShotNumbers) target t
        newLevel = Level i target newFinished
        newAnim = UIAnimation (UIEvolutions j upDown newLeft) k l
    putGameState
      $ assert (isFinished newAnim)
      $ GameState b newMultiplicator world futureWorld allShotNumbers newLevel newAnim s

{-# INLINABLE onMove #-}
onMove :: (MonadState AppState m, MonadIO m)
       => [(ShipId, Coords Vel)]
       -> m ()
onMove accelerations = getGameState >>= \(GameState _ m@(Multiplicator mv) world a b c d e) ->
  liftIO getSystemTime >>= \t -> do
    let nextTime = addDuration (toSystemDuration m gameMotionPeriod) t
    putGameState $ GameState (Just nextTime) (Multiplicator (mv + 0.01)) (moveWorld t accelerations world) a b c d e
    onHasMoved t

{-# INLINABLE onHasMoved #-}
onHasMoved :: (MonadState AppState m)
           => Time Point System
           -> m ()
onHasMoved t =
  shipParticleSystems t >>= addParticleSystems >> getGameState
    >>= \(GameState b m (World balls ships space systems) futureWorld shotNums (Level i target finished) anim s) -> do
    let allCollisions =
          concatMap
          (\(BattleShip _ _ _ safeTime collisions) ->
            if isNothing safeTime
              then collisions
              else [])
          ships
        remainingBalls = filter (`notElem` allCollisions) balls
        newWorld = World remainingBalls ships space systems
        finishIfOneShipCollides =
          case allCollisions of
            [] -> Nothing
            _  ->
              let msg = "collision with " <> showListOrSingleton (map getNumber allCollisions)
              in Just $ LevelFinished (Lost msg) t InfoMessage
        newLevel = Level i target (finished <|> finishIfOneShipCollides)
    putGameState $ assert (isFinished anim) $ GameState b m newWorld futureWorld shotNums newLevel anim s

{-# INLINABLE updateUIAnim #-}
updateUIAnim :: (MonadState AppState m)
             => Time Point System -> m()
updateUIAnim t =
  getGameState >>= \(GameState _ m curWorld futWorld j k (UIAnimation evolutions _ it) s) -> do
    let nextIt@(Iteration _ nextFrame) = nextIteration it
        (world, worldAnimDeadline) =
          maybe
            (futWorld, Nothing)
            (\dt ->
             (curWorld, Just $ addDuration dt t))
            $ getDeltaTime evolutions nextFrame
        anims = UIAnimation evolutions worldAnimDeadline nextIt
    putGameState $ GameState Nothing m world futWorld j k anims s
    when (isFinished anims) $ startGameState t
