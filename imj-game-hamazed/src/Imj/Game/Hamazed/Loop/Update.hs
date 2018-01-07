{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Loop.Update
      ( update
      ) where

import           Imj.Prelude

import           Data.Maybe( catMaybes, isNothing )

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Parameters
import           Imj.Game.Hamazed.Types
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
import           Imj.Graphics.UI.RectContainer
import           Imj.Util


-- | Updates the state. It needs IO just to generate random numbers in case
-- 'Event' is 'StartLevel'
{-# INLINABLE update #-}
update :: GameParameters
       -- ^ 'World' creation parameters, used in case the 'Event' is 'StartLevel'.
       -> GameState
       -- ^ The current state
       -> Event
       -- ^ The 'Event' that should be handled here.
       -> IO GameState
update
 params
 state@(GameState b world@(World c d space systems e)
                  futWorld f h@(Level level target mayLevelFinished) anim) = \case
  StartLevel nextLevel ->
    mkInitialState params nextLevel (Just state) >>= \case
      Left err -> error err
      Right s -> return s
  (Timeout (Deadline gt AnimateUI)) ->
    return $ updateAnim gt state
  (Timeout (Deadline _ DisplayContinueMessage)) ->
    return $ case mayLevelFinished of
      Just (LevelFinished stop finishTime _) ->
        let newLevel = Level level target (Just $ LevelFinished stop finishTime ContinueMessage)
        in GameState b world futWorld f newLevel anim
      Nothing -> state
  (Timeout (Deadline k AnimateParticleSystems)) -> do
    let newSystems = mapMaybe (\a -> if shouldUpdate a k
                                        then updateParticleSystem a
                                        else Just a) systems
    return $ GameState b (World c d space newSystems e) futWorld f h anim
  (Timeout (Deadline gt MoveFlyingItems)) -> do
    let movedState = GameState (Just $ addDuration gameMotionPeriod gt) (moveWorld gt world) futWorld f h anim
    return $ onHasMoved movedState gt
  Action Laser dir ->
    if isFinished anim
      then
        onLaser state dir <$> getSystemTime
      else
        return state
  Action Ship dir ->
    return $ accelerateShip' dir state
  (Interrupt _) ->
    return state
  EndGame -> -- TODO instead, go back to game configuration ?
    return state


onLaser :: GameState
        -> Direction
        -> SystemTime
        -> GameState
onLaser
  (GameState b world@(World _ (BattleShip posspeed ammo safeTime collisions)
                     space systems e@(InTerminal _ viewMode view))
             futureWorld g level@(Level i target finished)
             (UIAnimation (UIEvolutions j upDown left) k l))
  dir t =
  let (remainingBalls, destroyedBalls, maybeLaserRay, newAmmo) =
        laserEventAction dir world
      outerSpaceParticleSystems_ =
         if null destroyedBalls
           then
             maybe [] (outerSpaceParticleSystems t world) maybeLaserRay
           else
            []
      newSystems =
        destroyedNumbersParticleSystems (Left t) dir world destroyedBalls
        ++ maybe [] (`laserParticleSystems` t) maybeLaserRay
        ++ outerSpaceParticleSystems_

      newWorld = World remainingBalls (BattleShip posspeed newAmmo safeTime collisions)
                       space (newSystems ++ systems) e
      destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
      allShotNumbers = g ++ destroyedNumbers
      newLeft =
        if null destroyedNumbers && ammo == newAmmo
          then
            left
          else
            let frameSpace = mkRectContainerWithTotalArea view
                infos = mkLeftInfo Normal newAmmo allShotNumbers level
                (horizontalDist, verticalDist) = computeViewDistances viewMode
                (_, _, leftMiddle, _) = getSideCentersAtDistance frameSpace horizontalDist verticalDist
            in mkTextAnimRightAligned leftMiddle leftMiddle infos 1 0 -- 0 duration, since animation is over anyway
      newFinished = finished <|> checkTargetAndAmmo newAmmo (sum allShotNumbers) target t
      newLevel = Level i target newFinished
      newAnim = UIAnimation (UIEvolutions j upDown newLeft) k l
  in assert (isFinished newAnim) $ GameState b newWorld futureWorld allShotNumbers newLevel newAnim

-- | The world has moved, so we update it.
onHasMoved :: GameState
           -> KeyTime
           -> GameState
onHasMoved
  (GameState b world@(World balls ship@(BattleShip _ _ safeTime collisions) space systems e)
             futureWorld shotNums (Level i target finished) anim)
  keyTime@(KeyTime t) =
  let newSystems = shipParticleSystems world keyTime
      remainingBalls =
        if isNothing safeTime
          then
            filter (`notElem` collisions) balls
          else
            balls
      newWorld = World remainingBalls ship space (newSystems ++ systems) e
      finishIfShipCollides =
        maybe
          (case map (\(Number _ n) -> n) collisions of
            [] -> Nothing
            l  -> Just $ LevelFinished (Lost $ "collision with " <> showListOrSingleton l) t InfoMessage )
          (const Nothing)
            safeTime
      newLevel = Level i target (finished <|> finishIfShipCollides)
  in assert (isFinished anim) $ GameState b newWorld futureWorld shotNums newLevel anim

outerSpaceParticleSystems :: SystemTime
                          -> World
                          -> LaserRay Actual
                          -> [ParticleSystem]
outerSpaceParticleSystems t world@(World _ _ space _ _) ray@(LaserRay dir _ _) =
  let laserTarget = afterEnd ray
      char = materialChar Wall
  in  case location laserTarget space of
        InsideWorld -> []
        OutsideWorld ->
          if distanceToSpace laserTarget space > 0
            then
              let color _fragment _level _frame =
                    if 0 == _fragment `mod` 2
                      then
                        cycleOuterColors1 $ quot _frame 4
                      else
                        cycleOuterColors2 $ quot _frame 4
                  pos = translateInDir dir laserTarget
                  (speedAttenuation, nRebounds) = (0.3, 3)
              in outerSpaceParticleSystems' world NegativeWorldContainer pos
                   dir speedAttenuation nRebounds color char t
            else
              let color _fragment _level _frame =
                    if 0 == _fragment `mod` 3
                      then
                        cycleWallColors1 $ quot _frame 4
                      else
                        cycleWallColors2 $ quot _frame 4
                  (speedAttenuation, nRebounds) = (0.4, 5)
              in outerSpaceParticleSystems' world (WorldScope Wall) laserTarget
                   dir speedAttenuation nRebounds color char t

outerSpaceParticleSystems' :: World
                           -> Scope
                           -> Coords Pos
                           -> Direction
                           -> Float
                           -> Int
                           -> (Int -> Int -> Frame -> LayeredColor)
                           -> Char
                           -> SystemTime
                           -> [ParticleSystem]
outerSpaceParticleSystems' world scope afterLaserEndPoint dir speedAttenuation nRebounds colorFuncs char t =
  let speed = scalarProd 0.8 $ speed2vec $ coordsForDirection dir
      envFuncs = envFunctions world scope
  in  fragmentsFreeFallWithReboundsThenExplode
        speed afterLaserEndPoint speedAttenuation nRebounds colorFuncs char
        (Speed 1) envFuncs (Left t)


laserParticleSystems :: LaserRay Actual
           -> SystemTime
           -> [ParticleSystem]
laserParticleSystems ray t =
  catMaybes [laserShot ray cycleLaserColors (Left t)]


accelerateShip' :: Direction -> GameState -> GameState
accelerateShip' dir (GameState c (World wa ship wc wd we) b f g h) =
  let newShip = accelerateShip dir ship
      world = World wa newShip wc wd we
  in GameState c world b f g h

updateAnim :: KeyTime -> GameState -> GameState
updateAnim kt (GameState _ curWorld futWorld j k (UIAnimation evolutions _ it)) =
  let nextIt@(Iteration _ nextFrame) = nextIteration it
      (world, gameDeadline, worldAnimDeadline) =
        maybe
          (futWorld , Just kt, Nothing)
          (\dt ->
           (curWorld, Nothing, Just $ addDuration (floatSecondsToDiffTime dt) kt))
          $ getDeltaTime evolutions nextFrame
      wa = UIAnimation evolutions worldAnimDeadline nextIt
  in GameState gameDeadline world futWorld j k wa
