{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.World
    ( -- * Level
      {-| There are 12 levels in Hamazed, numbered from 1 to 12.
      -}
      Level(..)
      -- ** Level termination
      {-| [Target number]
      Each level has a different /target number/ which represents the sum of shot
      'Number's that should be reached to finish the 'Level'.

       A 'Level' is finished once the sum of shot 'Number's amounts to the /target number/. -}
    , checkTargetAndAmmo
    -- * World
    {- | A 'World' brings together:

    * game elements : 'Space', 'BattleShip' and 'Number's,
    * drawing elements: 'ParticleSystem's,
    * terminal-awareness : 'InTerminal'
    -}
    , World(..)
    -- ** World size
      {-|
      The 'World' size decreases with increasing 'Level' numbers.

      'worldSizeFromLevel' gives the 'Size' of the 'World' based on
      the 'Level' number and the 'WorldShape':
      -}
    , worldSizeFromLevel
    -- ** Update World
    -- | Every 'gameMotionPeriod' seconds, the positions of 'BattleShip' and 'Numbers'
    -- are updated according to their speeds:
    , gameMotionPeriod
    , moveWorld
    -- ** Draw World
    , drawWorld
    -- ** World utilities
    -- | 'laserEventAction' returns the effect a laser shot it has on the 'World'.
    , laserEventAction
    -- * Space
    {-| 'Space' describes the environment in which 'Number's and the 'BattleShip'
    live.

    It can be composed of 'Air', where 'BattleShip' and 'Number's are free to move, and of
    'Wall'.
    -}
    , Space
    , Material(..)
      -- ** Simple creation
    , mkEmptySpace
      -- ** Randomized creation
      {-| 'mkRandomlyFilledSpace' places 'Wall's at random and discards resulting
      'Space's which have more than one 'Air' connected component.

      This way, the 'BattleShip' is guaranteed to be able to reach any part of
      the 'Space', and 'Number's.

      Generating a big 'Space' with a very small block size can
      be computationnaly expensive because the probability to have a single 'Air'
      connected component drops very fast (probably at least exponentially)
      towards zero with increasing sizes. -}
    , mkRandomlyFilledSpace
    , RandomParameters(..)
    , Strategy(..)
      -- ** Collision detection
      {- | 'location' is the standard collision detection function that considers
      that being outside the world means being in collision.

      'scopedLocation' provides more options with the use of 'Scope' to
      defines the collision detection scopes.
       -}
    , location
    , scopedLocation
    , Scope(..)
      -- ** Draw
    , drawSpace
      -- * Movable items
      -- | A movable item's 'PosSpeed' is updated using 'updateMovableItem'
      -- at each 'MoveFlyingItems' event:
    , updateMovableItem
      -- ** BattleShip
    , BattleShip(..)
    -- ** Number
    -- | 'Number's can be shot by the 'BattleShip' to finish the 'Level'.
    --
    -- Number can collide with the 'BattleShip', hence triggering colorfull
    -- 'ParticleSystem' explosions.
    --
    -- 'Number's never change speed, except when they rebound on 'Wall's, of course.
    , Number(..)
    -- * UI
    {- | UI elements around the 'World' are:

    * a 'RectContainer' created by 'mkRectContainerWithTotalArea' to visually delimit the 'World'
    * 'ColorString' information, placed around the 'RectContainer':

        * Up: 'Level' target
        * Left: remaining ammunitions / shot 'Number's
        * Down: 'Level' number
    -}
    , mkRectContainerWithTotalArea
    -- ** Inter-level animations
    , module Imj.Graphics.UI.Animation
    -- * Secondary types
    , WallDistribution(..), WorldShape(..), ViewMode(..)
    , LevelFinished(..)
    , LevelOutcome(..)
    -- * Reexports
    , module Imj.Graphics.Render
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Map.Strict(fromAscList, assocs, insert)
import           Data.List(find, elem)
import           Data.Text(pack)

import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Graphics.ParticleSystem.Design.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Draw
import           Imj.Game.Hamazed.World.Number
import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.Render
import           Imj.Graphics.ParticleSystem
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.RectContainer
import           Imj.Physics.Discrete.Collision


-- | Moves elements of game logic ('Number's, 'BattleShip').
--
-- Note that 'ParticleSystem's are not updated.
moveWorld :: [(ShipId, Coords Vel)]
          -> [ShipId]
          -> World
          -> World
moveWorld accelerations shipsLosingArmor (World balls ships space rs anims e) =
  let newBalls = map (\(Number ps n) -> Number (updateMovableItem space ps) n) balls
      moveShip (sid, BattleShip name (PosSpeed prevPos oldSpeed) ammo status _) =
        let collisions =
              if shipIsAlive status
                then
                  getColliding pos newBalls
                else
                  []
            destroyedOr x =
              if null collisions
                then x
                else Destroyed
            newStatus =
              case status of
                Destroyed -> Destroyed
                Armored ->
                  if sid `elem` shipsLosingArmor
                    then
                      destroyedOr Unarmored
                    else
                      Armored
                Unarmored -> destroyedOr Unarmored
            newSpeed =
              maybe oldSpeed (\(_,acc) -> sumCoords acc oldSpeed)
              -- induces a square complexity, but if the number of ships is small it's ok.
              $ find ((==) sid . fst) accelerations
            newPosSpeed@(PosSpeed pos _) = updateMovableItem space $ PosSpeed prevPos newSpeed
        in (sid,BattleShip name newPosSpeed ammo newStatus collisions)
      -- using fromAscList ecause the keys are unchanged.
      newShips = fromAscList $ map moveShip $ assocs ships
  in World newBalls newShips space rs anims e

-- | Computes the effect of a laser shot on the 'World'.
laserEventAction :: (MonadState AppState m)
                 => ShipId
                 -> Direction
                 -- ^ The direction of the laser shot
                 -> Time Point System
                 -> m [Number]
                 -- ^ 'Number's destroyed
laserEventAction shipId dir t =
  getWorld >>= \(World balls ships space rs d e) -> do
    let ship@(BattleShip _ (PosSpeed shipCoords _) ammo status _) = findShip shipId ships
        (maybeLaserRayTheoretical, newAmmo) =
          if ammo > 0 && shipIsAlive status
            then
              (Just $ shootLaserWithOffset shipCoords dir Infinite (`location` space)
             , pred ammo)
            else
              (Nothing
             , ammo)

        ((remainingBalls, destroyedBalls), maybeLaserRay) =
           maybe
             ((balls,[]), Nothing)
             (\r -> Just <$> computeActualLaserShot balls (\(Number (PosSpeed pos _) _) -> pos)
                                                    r DestroyFirstObstacle)
               maybeLaserRayTheoretical
        newShips = insert shipId (ship { getAmmo = newAmmo }) ships
    putWorld $ World remainingBalls newShips space rs d e

    let tps = systemTimePointToParticleSystemTimePoint t
    outerSpaceParticleSystems_ <-
      if null destroyedBalls
        then
          maybe (return []) (outerSpaceParticleSystems tps shipId) maybeLaserRay
        else
          return []
    newSystems <- destroyedNumbersParticleSystems tps shipId dir destroyedBalls
    laserSystems <- maybe (return []) (laserParticleSystems tps shipId) maybeLaserRay
    addParticleSystems $ concat [newSystems, laserSystems, outerSpaceParticleSystems_]

    return destroyedBalls


outerSpaceParticleSystems :: (MonadState AppState m)
                          => Time Point ParticleSyst
                          -> ShipId
                          -> LaserRay Actual
                          -> m [Prioritized ParticleSystem]
outerSpaceParticleSystems t shipId ray@(LaserRay dir _ _) = getPlayer shipId >>= maybe
  (return [])
  (\(Player _ _ (PlayerColors _ cycles)) -> do
    world <- getWorld
    let space = getWorldSpace world
        laserTarget = afterEnd ray
        char = materialChar Wall
    case location laserTarget space of
          InsideWorld -> return []
          OutsideWorld ->
            if distanceToSpace laserTarget space > 0
              then do
                let color _fragment _level _frame =
                      if 0 == _fragment `mod` 2
                        then
                          cycleColors (outer1 cycles) $ quot _frame 4
                        else
                          cycleColors (outer2 cycles) $ quot _frame 4
                    pos = translateInDir dir laserTarget
                    (speedAttenuation, nRebounds) = (0.3, 3)
                mode <- getViewMode
                screen <- getCurScreen
                case scopedLocation world mode screen NegativeWorldContainer pos of
                    InsideWorld -> outerSpaceParticleSystems' NegativeWorldContainer pos
                                    dir speedAttenuation nRebounds color char t
                    OutsideWorld -> return []
              else do
                let color _fragment _level _frame =
                      if 0 == _fragment `mod` 3
                        then
                          cycleColors (wall1 cycles) $ quot _frame 4
                        else
                          cycleColors (wall2 cycles) $ quot _frame 4
                    (speedAttenuation, nRebounds) = (0.4, 5)
                outerSpaceParticleSystems' (WorldScope Wall) laserTarget
                     dir speedAttenuation nRebounds color char t)

outerSpaceParticleSystems' :: (MonadState AppState m)
                           => Scope
                           -> Coords Pos
                           -> Direction
                           -> Float
                           -> Int
                           -> (Int -> Int -> Frame -> LayeredColor)
                           -> Char
                           -> Time Point ParticleSyst
                           -> m [Prioritized ParticleSystem]
outerSpaceParticleSystems' scope afterLaserEndPoint dir speedAttenuation nRebounds colorFuncs char t = do
  envFuncs <- envFunctions scope
  let speed = scalarProd 0.8 $ speed2vec $ coordsForDirection dir
  return
    $ map (Prioritized particleSystDefaultPriority)
    $ fragmentsFreeFallWithReboundsThenExplode
      speed afterLaserEndPoint speedAttenuation nRebounds colorFuncs char
      (Speed 1) envFuncs t

laserParticleSystems :: (MonadState AppState m)
                     => Time Point ParticleSyst
                     -> ShipId
                     -> LaserRay Actual
                     -> m [Prioritized ParticleSystem]
laserParticleSystems t shipId ray = getPlayer shipId >>= maybe
  (return [])
  (\(Player _ _ (PlayerColors _ cycles)) ->
    return $ catMaybes
      [Prioritized particleSystLaserPriority <$> laserShot ray (cycleColors $ laser cycles) t])


checkTargetAndAmmo :: Int
                   -- ^ Remaining ammo
                   -> Int
                   -- ^ The current sum of all shot 'Numbers'
                   -> LevelTarget
                   -- ^ The 'Level' 's target number.
                   -> Time Point System
                   -- ^ The current time
                   -> Maybe LevelFinished
checkTargetAndAmmo ammo currentNumber (LevelTarget goal constraint) t =
    maybe Nothing (\stop -> Just $ LevelFinished stop t InfoMessage) allChecks
  where
    allChecks = checkSum <|> checkAmmo

    checkSum = case compare currentNumber goal of
      EQ -> Just Won
      LT -> Nothing
      GT -> case constraint of
        CanOvershoot -> Nothing
        CannotOvershoot -> Just $ Lost $ pack $ show currentNumber ++ " is bigger than " ++ show goal
    checkAmmo
      | ammo <= 0 = Just $ Lost $ pack "no ammo left"
      | otherwise = Nothing
