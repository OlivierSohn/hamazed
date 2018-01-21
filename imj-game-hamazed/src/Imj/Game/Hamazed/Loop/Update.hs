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
updateAppState :: (MonadState AppState m, MonadReader e m, Canvas e, MonadIO m)
               => Event
               -- ^ The 'Event' that should be handled here.
               -> m ()
updateAppState evt =
  getGame >>= \(Game _ params state@(GameState _ _ _ _ _ anim _)) -> case evt of
    Configuration char -> updateGameParamsFromChar char
    StartGame          -> putUserIntent Play >> updateAppState (StartLevel firstLevel)
    EndGame            -> onEndGame
    StartLevel nextLevel -> do
      (Screen sz _) <- getCurScreen
      mkInitialState params sz nextLevel (Just state)
        >>= putGameState
    (Timeout (Deadline gt _ AnimateUI)) -> updateUIAnim gt
    (Timeout (Deadline gt _ MoveFlyingItems)) -> onMove gt
    (Timeout (Deadline _ _ (AnimateParticleSystem key))) ->
      liftIO getSystemTime >>= updateOneParticleSystem key
    Action target dir -> do
      when (isFinished anim) $ case target of
        Laser -> liftIO getSystemTime >>= onLaser dir
        Ship  -> accelerateShip' dir
    (Timeout (Deadline _ _ DisplayContinueMessage)) -> onContinueMessage
    _ -> error $ "The caller should handle:" ++ show evt

onEndGame :: (MonadState AppState m, Canvas e, MonadReader e m, MonadIO m) => m ()
onEndGame =
  getGameParameters >>= \params -> do
    getTargetSize
      >>= liftIO . initialGameState params
      >>= putGame . Game Configure params
    return ()

updateGameParamsFromChar :: (MonadState AppState m)
                         => Char
                         -> m ()
updateGameParamsFromChar c =
  getGameParameters >>= \p@(GameParameters shape wallType mode) ->
    putGameParameters $
      case c of
        '1' -> GameParameters Square wallType mode
        '2' -> GameParameters Rectangle2x1 wallType mode
        'e' -> GameParameters shape None mode
        'r' -> GameParameters shape Deterministic mode
        't' -> GameParameters shape (Random $ RandomParameters minRandomBlockSize StrictlyOneComponent) mode
        'd' -> GameParameters shape wallType CenterSpace
        'f' -> GameParameters shape wallType CenterShip
        _ -> p

onContinueMessage :: (MonadState AppState m)
                  => m ()
onContinueMessage =
  getGameState >>= \(GameState a b c d (Level n target mayFinished) e f) -> do
    case mayFinished of
      Just (LevelFinished stop finishTime _) -> do
        let newLevel = Level n target (Just $ LevelFinished stop finishTime ContinueMessage)
        putGameState $ GameState a b c d newLevel e f
      Nothing -> return ()

startGameState :: (MonadState AppState m)
               => Time Point System
               -> m ()
startGameState t =
  getGameState >>= \(GameState _ world world' b d e f) ->
    putGameState
      $ GameState (Just $ systemTimePointToGameTimePoint t) (startWorld t world) (startWorld t world') b d e f

{-# INLINABLE onLaser #-}
onLaser :: (MonadState AppState m)
        => Direction
        -> Time Point System
        -> m ()
onLaser dir t = do
  destroyedBalls <- laserEventAction dir t

  getGameState >>= \(GameState b (World _ (BattleShip _ ammo _ _) (Space _ sz _) _)
                               futureWorld g level@(Level i target finished)
                   (UIAnimation (UIEvolutions j upDown left) k l) s) -> do
    (Screen _ center) <- getCurScreen
    mode <- getMode
    newWorld@(World _ (BattleShip _ newAmmo _ _) _ _) <- getWorld
    let destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
        allShotNumbers = g ++ destroyedNumbers
        newLeft =
          if null destroyedNumbers && ammo == newAmmo
            then
              left
            else
              let frameSpace = mkRectContainerWithCenterAndInnerSize center sz
                  (horizontalDist, verticalDist) = computeViewDistances mode
                  (_, _, leftMiddle, _) = getSideCenters $ mkRectContainerAtDistance frameSpace horizontalDist verticalDist
                  infos = mkLeftInfo Normal newAmmo allShotNumbers level
              in mkTextAnimRightAligned leftMiddle leftMiddle infos 1 0 -- 0 duration, since animation is over anyway
        newFinished = finished <|> checkTargetAndAmmo newAmmo (sum allShotNumbers) target t
        newLevel = Level i target newFinished
        newAnim = UIAnimation (UIEvolutions j upDown newLeft) k l
    putGameState
      $ assert (isFinished newAnim)
      $ GameState b newWorld futureWorld allShotNumbers newLevel newAnim s

{-# INLINABLE onMove #-}
onMove :: (MonadState AppState m)
       => Time Point System
       -> m ()
onMove t =
  getGameState >>= \(GameState _ world a b c d e) -> do
    let tgame = systemTimePointToGameTimePoint t
        nextTime = Just $ addDuration gameMotionPeriod tgame
        newWorld = moveWorld t world
    putGameState $ GameState nextTime newWorld a b c d e
    onHasMoved t

{-# INLINABLE onHasMoved #-}
onHasMoved :: (MonadState AppState m)
           => Time Point System
           -> m ()
onHasMoved t = do
  shipParticleSystems t >>= addParticleSystems
  getGameState >>= \(GameState b (World balls ship@(BattleShip _ _ safeTime collisions) space systems)
             futureWorld shotNums (Level i target finished) anim s) -> do
    let remainingBalls =
          if isNothing safeTime
            then
              filter (`notElem` collisions) balls
            else
              balls
        newWorld = World remainingBalls ship space systems
        finishIfShipCollides =
          maybe
            (case map (\(Number _ n) -> n) collisions of
              [] -> Nothing
              l  -> Just $ LevelFinished (Lost $ "collision with " <> showListOrSingleton l) t InfoMessage )
            (const Nothing)
              safeTime
        newLevel = Level i target (finished <|> finishIfShipCollides)
    putGameState $ assert (isFinished anim) $ GameState b newWorld futureWorld shotNums newLevel anim s

{-# INLINABLE accelerateShip' #-}
accelerateShip' :: (MonadState AppState m)
                => Direction -> m ()
accelerateShip' dir =
  getWorld >>= \(World a ship b c) ->
    putWorld $ World a (accelerateShip dir ship) b c

{-# INLINABLE updateUIAnim #-}
updateUIAnim :: (MonadState AppState m)
             => Time Point System -> m()
updateUIAnim t =
  getGameState >>= \(GameState _ curWorld futWorld j k (UIAnimation evolutions _ it) s) -> do
    let nextIt@(Iteration _ nextFrame) = nextIteration it
        (world, worldAnimDeadline) =
          maybe
            (futWorld, Nothing)
            (\dt ->
             (curWorld, Just $ addDuration dt t))
            $ getDeltaTime evolutions nextFrame
        anims = UIAnimation evolutions worldAnimDeadline nextIt
    putGameState $ GameState Nothing world futWorld j k anims s
    when (isFinished anims) $ startGameState t
