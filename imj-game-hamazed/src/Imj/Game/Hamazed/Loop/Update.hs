{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Loop.Update
      ( update
      ) where

import           Imj.Prelude

import           Data.Maybe( catMaybes, isNothing )

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
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.Animation
import           Imj.Graphics.UI.RectContainer
import           Imj.Physics.Discrete.Collision
import           Imj.Util


-- | Updates the state. It needs IO just to generate random numbers in case
-- 'Event' is 'StartLevel'
{-# INLINABLE update #-}
update :: GameParameters
       -- ^ 'World' creation parameters, used in case the 'Event' is 'StartLevel'.
       -> GameState
       -- ^ The current state
       -> TimestampedEvent
       -- ^ The 'TimestampedEvent' that should be handled here.
       -> IO GameState
update
 params
 state@(GameState b world@(World c d space animations e@(InTerminal mayTermWindow curUpperLeft))
                  futWorld f h@(Level level target mayLevelFinished) anim)
 (TimestampedEvent event t) =
  case event of
    StartLevel nextLevel ->
      mkInitialState params nextLevel (Just state) >>= \case
        Left err -> error err
        Right s -> return s
    (Timeout (Deadline _ AnimateUI)) ->
      return $ updateAnim t state
    (Timeout (Deadline _ DisplayContinueMessage)) ->
      return $ case mayLevelFinished of
        Just (LevelFinished stop finishTime _) ->
          let newLevel = Level level target (Just $ LevelFinished stop finishTime ContinueMessage)
          in GameState b world futWorld f newLevel anim
        Nothing -> state
    (Timeout (Deadline _ Animate)) -> do
      let worldCorner = translate' 1 1 curUpperLeft
          newAnimations = updateAnimations (getKeyTime event) space mayTermWindow worldCorner animations
      return $ GameState b (World c d space newAnimations e) futWorld f h anim
    (Timeout (Deadline gt MoveFlyingItems)) -> do
      let movedState = GameState (Just $ addDuration gameMotionPeriod gt) (moveWorld t world) futWorld f h anim
      return $ onHasMoved movedState (KeyTime t)
    Action Laser dir ->
      return $ if isFinished anim
        then
          onLaser state dir (KeyTime t)
        else
          state
    Action Ship dir ->
      return $ accelerateShip' dir state
    (Interrupt _) ->
      return state
    EndGame -> -- TODO instead, go back to game configuration ?
      return state


onLaser :: GameState
        -> Direction
        -> KeyTime
        -> GameState
onLaser
  (GameState b world@(World _ (BattleShip posspeed ammo safeTime collisions)
                     space animations e@(InTerminal mayTermWindow curUpperLeft))
             futureWorld g (Level i target finished)
             (UIAnimation (UIEvolutions j upDown left) k l))
  dir keyTime@(KeyTime t) =
  let (remainingBalls, destroyedBalls, maybeLaserRay, newAmmo) =
        laserEventAction dir world
      outerSpaceAnims_ =
         if null destroyedBalls
           then
             maybe [] (outerSpaceAnims keyTime space) maybeLaserRay
           else
            []
      worldCorner = translate' 1 1 curUpperLeft
      newAnimations =
        updateAnimations (Just keyTime) space mayTermWindow worldCorner
          $ destroyedNumbersAnimations keyTime dir destroyedBalls
          ++ maybe [] (`laserAnims` keyTime) maybeLaserRay
          ++ outerSpaceAnims_

      newWorld = World remainingBalls (BattleShip posspeed newAmmo safeTime collisions)
                       space (newAnimations ++ animations) e
      destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
      allShotNumbers = g ++ destroyedNumbers
      newLeft =
        if null destroyedNumbers && ammo == newAmmo
          then
            left
          else
            let frameSpace = mkWorldContainer world
                infos = mkLeftInfo Normal newAmmo allShotNumbers
                (_, _, leftMiddle, _) = getSideCentersAtDistance frameSpace 3 2
            in mkTextAnimRightAligned leftMiddle leftMiddle infos 0 -- 0 duration, since animation is over anyway
      newFinished = finished <|> checkTargetAndAmmo newAmmo (sum allShotNumbers) target t
      newLevel = Level i target newFinished
      newAnim = UIAnimation (UIEvolutions j upDown newLeft) k l
  in assert (isFinished newAnim) $ GameState b newWorld futureWorld allShotNumbers newLevel newAnim

-- | The world has moved, so we update it.
onHasMoved :: GameState
           -> KeyTime
           -> GameState
onHasMoved
  (GameState b (World balls ship@(BattleShip _ _ safeTime collisions) space animations
                      e@(InTerminal mayTermWindow curUpperLeft))
             futureWorld shotNums (Level i target finished) anim)
  keyTime@(KeyTime t) =
  let worldCorner = translate' 1 1 curUpperLeft
      newAnimations = updateAnimations (Just keyTime) space mayTermWindow worldCorner
        $ shipAnims ship keyTime
      remainingBalls =
           if isNothing safeTime
             then
               filter (`notElem` collisions) balls
             else
               balls
      newWorld = World remainingBalls ship space (newAnimations ++ animations) e
      finishIfShipCollides =
        maybe
          (case map (\(Number _ n) -> n) collisions of
            [] -> Nothing
            l  -> Just $ LevelFinished (Lost $ "collision with " <> showListOrSingleton l) t InfoMessage )
          (const Nothing)
            safeTime
      newLevel = Level i target (finished <|> finishIfShipCollides)
  in assert (isFinished anim) $ GameState b newWorld futureWorld shotNums newLevel anim


outerSpaceAnims :: KeyTime
                -> Space
                -> LaserRay Actual
                -> [BoundedAnimation]
outerSpaceAnims k (Space _ sz _) ray@(LaserRay dir _) =
  let laserTarget = afterEnd ray
  in case onOuterBorder laserTarget sz of
       Just outDir -> outerSpaceAnims' k laserTarget $ assert (dir == outDir) dir
       Nothing -> []

outerSpaceAnims' :: KeyTime
                 -> Coords Pos
                 -> Direction
                 -> [BoundedAnimation]
outerSpaceAnims' keyTime@(KeyTime (MkSystemTime _ nanos)) fronteerPoint dir =
  let char = niceChar $ fromIntegral nanos -- cycle character every nano second
      speed = scalarProd 0.8 $ speed2vec $ coordsForDirection dir
      outerSpacePoint = translateInDir dir fronteerPoint
      anims = fragmentsFreeFall speed outerSpacePoint keyTime (Speed 1) char
  in map (`BoundedAnimation` TerminalWindow) anims


laserAnims :: LaserRay Actual
           -> KeyTime
           -> [BoundedAnimation]
laserAnims keyTime ray
 = [BoundedAnimation (laserAnimation keyTime ray) WorldFrame]


accelerateShip' :: Direction -> GameState -> GameState
accelerateShip' dir (GameState c (World wa ship wc wd we) b f g h) =
  let newShip = accelerateShip dir ship
      world = World wa newShip wc wd we
  in GameState c world b f g h

updateAnim :: SystemTime -> GameState -> GameState
updateAnim t (GameState _ curWorld futWorld j k (UIAnimation evolutions _ it)) =
  let nextIt@(Iteration _ nextFrame) = nextIteration it
      (world, gameDeadline, worldAnimDeadline) =
        maybe
          (futWorld , Just $ KeyTime t, Nothing)
          (\dt ->
           (curWorld, Nothing         , Just $ KeyTime $ addToSystemTime (floatSecondsToDiffTime dt) t))
          $ getDeltaTime evolutions nextFrame
      wa = UIAnimation evolutions worldAnimDeadline nextIt
  in GameState gameDeadline world futWorld j k wa

{-# INLINABLE updateAnimations #-}
updateAnimations :: Maybe KeyTime
                 -> Space
                 -> Maybe (Window Int)
                 -> Coords Pos
                 -> [BoundedAnimation]
                 -> [BoundedAnimation]
updateAnimations k space mayTermWindow worldCorner animations =
  let updateAnimation (BoundedAnimation a scope) =
        let interaction =
              scopedLocation space mayTermWindow worldCorner scope >>> \case
                InsideWorld  -> Stable
                OutsideWorld -> Mutation
        in case updateAnimationIfNeeded k interaction a of
          Nothing -> Nothing
          Just a' -> Just $ BoundedAnimation a' scope
  in catMaybes $ map updateAnimation animations
