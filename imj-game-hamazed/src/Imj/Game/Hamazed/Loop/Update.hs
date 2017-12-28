{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Loop.Update
      ( update
      ) where

import           Imj.Prelude

import           Data.Maybe( catMaybes )

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
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.Animation
import           Imj.Graphics.UI.RectContainer
import           Imj.Physics.Discrete.Collision


-- TODO simplify : no need to update animations if it's not an animation event,
-- when there are new animations, update just these, not the other
nextGameState :: GameState
              -> TimestampedEvent
              -> GameState
nextGameState
  (GameState b world@(World _ ship@(BattleShip posspeed ammo safeTime collisions)
                     space animations e@(InTerminal mayTermWindow curUpperLeft))
             futureWorld g (Level i target finished)
             (UIAnimation (UIEvolutions j upDown left) k l))
  te@(TimestampedEvent event t) =
  let (remainingBalls, destroyedBalls, maybeLaserRay, newAmmo) = eventAction event world
      keyTime = KeyTime t

      outerSpaceAnims_ =
         if null destroyedBalls
           then
             maybe [] (outerSpaceAnims keyTime space) maybeLaserRay
           else
            []

      newAnimations' =
            destroyedNumbersAnimations keyTime event destroyedBalls
         ++ shipAnims ship event
         ++ maybe [] (`laserAnims` keyTime) maybeLaserRay
         ++ outerSpaceAnims_
         ++ animations

      worldCorner = translate' 1 1 curUpperLeft
      newAnimations = updateAnimations (getKeyTime event) space mayTermWindow worldCorner newAnimations'

      newWorld = World remainingBalls (BattleShip posspeed newAmmo safeTime collisions) space newAnimations e
      destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
      allShotNumbers = g ++ destroyedNumbers
      newLeft =
        if null destroyedNumbers && ammo == newAmmo
          then
            left
          else
            let frameSpace = mkWorldContainer worldFrameColors world
                infos = mkLeftInfo Normal newAmmo allShotNumbers
                (_, _, leftMiddle) = getSideCentersAtDistance frameSpace 2
            in mkTextAnimRightAligned leftMiddle leftMiddle infos 0 -- 0 duration, since animation is over anyway
      newFinished = finished <|> isLevelFinished newWorld (sum allShotNumbers) target te
      newLevel = Level i target newFinished
      newAnim = UIAnimation (UIEvolutions j upDown newLeft) k l
  in assert (isFinished newAnim) $ GameState b newWorld futureWorld allShotNumbers newLevel newAnim


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


-- | Updates the state. It needs IO just to generate random numbers in case
-- 'Event' is 'StartLevel'
{-# INLINABLE update #-}
update :: GameParameters
       -- ^ 'World' creation parameters
       -- (will be used in case the 'Event' is 'StartLevel')
       -> GameState
       -- ^ The current state
       -> TimestampedEvent
       -- ^ The 'TimestampedEvent' that should be handled here.
       -> IO GameState
update
 params
 state@(GameState b world futWorld f h@(Level level target mayLevelFinished) anim)
 te@(TimestampedEvent event t) =
  case event of
    StartLevel nextLevel ->
      mkInitialState params nextLevel (Just state) >>= \case
        Left err -> error err
        Right s -> return s
    (Interrupt _) ->
      return state
    (Timeout (Deadline _ AnimateUI)) ->
      return $ updateAnim t state
    (Timeout (Deadline _ DisplayContinueMessage)) ->
      return $ case mayLevelFinished of
        Just (LevelFinished stop finishTime _) ->
          let newLevel = Level level target (Just $ LevelFinished stop finishTime ContinueMessage)
          in GameState b world futWorld f newLevel anim
        Nothing -> state
    (Timeout (Deadline gt MoveFlyingItems)) -> do
        let newState = GameState (Just $ addDuration gameMotionPeriod gt) (updateWorld t world) futWorld f h anim
        return $ nextGameState newState te
    Action Ship dir ->
      return $ accelerateShip' dir state
    _ ->
      return $ if isFinished anim
        then
          nextGameState state te
        else
          state

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
