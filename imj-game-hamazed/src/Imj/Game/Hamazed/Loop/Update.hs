{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Update
      ( update
      ) where

import           Imj.Prelude

import           Data.Map.Strict(updateWithKey, Map, empty, singleton, fromList, union, unions)
import           Data.Maybe(isNothing)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Parameters
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World
import           Imj.Game.Hamazed.World.Number
import           Imj.Game.Hamazed.World.Ship
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Physics.Discrete.Collision
import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Graphics.ParticleSystem.Design.Update
import           Imj.Graphics.ParticleSystem
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.UI.RectContainer
import           Imj.Util


-- | Updates the game state. It needs IO just to generate random numbers in case
-- 'Event' is 'StartLevel'
{-# INLINABLE update #-}
update :: (MonadState AppState m, MonadReader e m, Canvas e, MonadIO m)
       => Event
       -- ^ The 'Event' that should be handled here.
       -> m ()
update evt =
  getGame >>=
    \(Game mode params
      state@(GameState b world@(World c d space systems) futWorld f h@(Level level target mayLevelFinished) anim s)) -> do
    t <- liftIO getSystemTime
    let tps = systemTimePointToParticleSystemTimePoint t
        onConfigParams x =
          getTargetSize
            >>= liftIO . initialGameState x
            >>= putGame . Game Configure x
        getNewState = case evt of
          StartLevel nextLevel -> do
            (Screen sz _) <- getCurScreen
            mkInitialState params sz nextLevel (Just state) >>= \case
              Left err -> error err
              Right st -> return st
          (Timeout (Deadline gt _ AnimateUI)) -> do
            let st@(GameState _ _ _ _ _ anims _) = updateAnim gt state
            if isFinished anims
              then return $ startGameState t st
              else return st
          (Timeout (Deadline _ _ DisplayContinueMessage)) ->
            return $ case mayLevelFinished of
              Just (LevelFinished stop finishTime _) ->
                let newLevel = Level level target (Just $ LevelFinished stop finishTime ContinueMessage)
                in GameState b world futWorld f newLevel anim s
              Nothing -> state
          (Timeout (Deadline _ _ (AnimateParticleSystem key))) -> do
            let newSystems =
                  updateWithKey
                    (\_ (Prioritized p a) -> fmap (Prioritized p) $ updateParticleSystem tps a)
                    key systems
            return $ GameState b (World c d space newSystems) futWorld f h anim s
          (Timeout (Deadline gt _ MoveFlyingItems)) -> do
            let tgame = systemTimePointToGameTimePoint gt
                movedState = GameState (Just $ addDuration gameMotionPeriod tgame) (moveWorld gt world) futWorld f h anim s
            onHasMoved movedState gt
          Action Laser dir ->
            if isFinished anim
              then
                onLaser state dir t
              else
                return state
          Action Ship dir ->
            return $ accelerateShip' dir state
          _ -> error $ "The caller should handle:" ++ show evt
    case evt of
      Configuration char -> onConfigParams $ updateFromChar char params
      StartGame          -> putUserIntent Play >> update (StartLevel firstLevel)
      EndGame            -> onConfigParams params -- go back to configuration mode.
      _ -> getNewState >>= putGame . Game mode params

onLaser :: (MonadState AppState m)
        => GameState
        -> Direction
        -> Time Point System
        -> m GameState
onLaser (GameState b world@(World _ (BattleShip posspeed ammo safeTime collisions)
                                  space@(Space _ sz _) systems)
                   futureWorld g level@(Level i target finished)
                   (UIAnimation (UIEvolutions j upDown left) k l) s)
  dir t = do
  mode <- getMode
  (Screen _ center) <- getCurScreen
  let tps = systemTimePointToParticleSystemTimePoint t
      (remainingBalls, destroyedBalls, maybeLaserRay, newAmmo) = laserEventAction dir world
  outerSpaceParticleSystems_ <-
    if null destroyedBalls
      then
        maybe (return empty) (outerSpaceParticleSystems tps world) maybeLaserRay
      else
        return empty
  newSystems <- destroyedNumbersParticleSystems tps dir world destroyedBalls
  laserSystems <- maybe (return empty) (`laserParticleSystems` tps) maybeLaserRay
  let allSystems = unions [newSystems, laserSystems, outerSpaceParticleSystems_, systems]
      newWorld = World remainingBalls (BattleShip posspeed newAmmo safeTime collisions)
                       space allSystems
      destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
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
  return $ assert (isFinished newAnim) $ GameState b newWorld futureWorld allShotNumbers newLevel newAnim s

-- | The world has moved, so we update it.
onHasMoved :: (MonadState AppState m)
           => GameState
           -> Time Point System
           -> m GameState
onHasMoved
  (GameState b world@(World balls ship@(BattleShip _ _ safeTime collisions) space systems)
             futureWorld shotNums (Level i target finished) anim s)
  t = do
  newSystems <- shipParticleSystems world t
  let remainingBalls =
        if isNothing safeTime
          then
            filter (`notElem` collisions) balls
          else
            balls
      newWorld = World remainingBalls ship space (union newSystems systems)
      finishIfShipCollides =
        maybe
          (case map (\(Number _ n) -> n) collisions of
            [] -> Nothing
            l  -> Just $ LevelFinished (Lost $ "collision with " <> showListOrSingleton l) t InfoMessage )
          (const Nothing)
            safeTime
      newLevel = Level i target (finished <|> finishIfShipCollides)
  return $ assert (isFinished anim) $ GameState b newWorld futureWorld shotNums newLevel anim s

outerSpaceParticleSystems :: (MonadState AppState m)
                          => Time Point ParticleSyst
                          -> World
                          -> LaserRay Actual
                          -> m (Map ParticleSystemKey (Prioritized ParticleSystem))
outerSpaceParticleSystems t world@(World _ _ space _) ray@(LaserRay dir _ _) = do
  let laserTarget = afterEnd ray
      char = materialChar Wall
  case location laserTarget space of
        InsideWorld -> return empty
        OutsideWorld ->
          if distanceToSpace laserTarget space > 0
            then do
              let color _fragment _level _frame =
                    if 0 == _fragment `mod` 2
                      then
                        cycleOuterColors1 $ quot _frame 4
                      else
                        cycleOuterColors2 $ quot _frame 4
                  pos = translateInDir dir laserTarget
                  (speedAttenuation, nRebounds) = (0.3, 3)
              mode <- getMode
              screen <- getCurScreen
              case scopedLocation world mode screen NegativeWorldContainer pos of
                  InsideWorld -> outerSpaceParticleSystems' world NegativeWorldContainer pos
                                  dir speedAttenuation nRebounds color char t
                  OutsideWorld -> return empty
            else do
              let color _fragment _level _frame =
                    if 0 == _fragment `mod` 3
                      then
                        cycleWallColors1 $ quot _frame 4
                      else
                        cycleWallColors2 $ quot _frame 4
                  (speedAttenuation, nRebounds) = (0.4, 5)
              outerSpaceParticleSystems' world (WorldScope Wall) laserTarget
                   dir speedAttenuation nRebounds color char t

outerSpaceParticleSystems' :: (MonadState AppState m)
                           => World
                           -> Scope
                           -> Coords Pos
                           -> Direction
                           -> Float
                           -> Int
                           -> (Int -> Int -> Frame -> LayeredColor)
                           -> Char
                           -> Time Point ParticleSyst
                           -> m (Map ParticleSystemKey (Prioritized ParticleSystem))
outerSpaceParticleSystems' world scope afterLaserEndPoint dir speedAttenuation nRebounds colorFuncs char t = do
  let speed = scalarProd 0.8 $ speed2vec $ coordsForDirection dir
  envFuncs <- envFunctions world scope
  keys <- forever takeKey
  return
    $ fromList
    $ zip keys
    $ fmap (Prioritized particleSystDefaultPriority)
    $ fragmentsFreeFallWithReboundsThenExplode
        speed afterLaserEndPoint speedAttenuation nRebounds colorFuncs char
        (Speed 1) envFuncs t


laserParticleSystems :: (MonadState AppState m)
                     => LaserRay Actual
                     -> Time Point ParticleSyst
                     -> m (Map ParticleSystemKey (Prioritized ParticleSystem))
laserParticleSystems ray t =
  maybe (return empty) (\v -> flip singleton v <$> takeKey)
    $ fmap (Prioritized particleSystLaserPriority)
    $ laserShot ray cycleLaserColors t


accelerateShip' :: Direction -> GameState -> GameState
accelerateShip' dir (GameState c (World wa ship wc wd) b f g h s) =
  let newShip = accelerateShip dir ship
      world = World wa newShip wc wd
  in GameState c world b f g h s

updateAnim :: Time Point System -> GameState -> GameState
updateAnim kt (GameState _ curWorld futWorld j k (UIAnimation evolutions _ it) s) =
  let nextIt@(Iteration _ nextFrame) = nextIteration it
      (world, worldAnimDeadline) =
        maybe
          (futWorld, Nothing)
          (\dt ->
           (curWorld, Just $ addDuration dt kt))
          $ getDeltaTime evolutions nextFrame
      wa = UIAnimation evolutions worldAnimDeadline nextIt
  in GameState Nothing world futWorld j k wa s
