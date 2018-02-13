{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.World
    (
      -- * Level
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
    , mkDeterministicallyFilledSpace
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
      -- ** Collision detection utilities
    , createRandomNonCollidingPosSpeed
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
    , GameOutcome(..)
    -- * Reexports
    , module Imj.Graphics.Render
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.List( find, elem )
import           Data.Text( pack )

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Draw
import           Imj.Game.Hamazed.World.Number
import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.Render
import           Imj.Graphics.ParticleSystem.Design.Types
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
      moveShip (BattleShip sid (PosSpeed prevPos oldSpeed) ammo safe _) =
        let newSafe =
              if safe
                then
                  not $ sid `elem` shipsLosingArmor
                else
                  False
            newSpeed =
              maybe oldSpeed (\(_,acc) -> sumCoords acc oldSpeed)
              -- induces a square complexity, but if the number of ships is small it's ok.
              $ find ((==) sid . fst) accelerations
            newPosSpeed@(PosSpeed pos _) = updateMovableItem space $ PosSpeed prevPos newSpeed
            collisions = getColliding pos newBalls
        in BattleShip sid newPosSpeed ammo newSafe collisions
  in World newBalls (map moveShip ships) space rs anims e

-- | Computes the effect of an laser shot on the 'World'.
laserEventAction :: (MonadState AppState m)
                 => ShipId
                 -> Direction
                 -- ^ The direction of the laser shot
                 -> Time Point System
                 -> m [Number]
                 -- ^ 'Number's destroyed
laserEventAction ship dir t =
  getWorld >>= \(World balls ships space rs d e) -> do
    let ((BattleShip _ shipPS@(PosSpeed shipCoords _) ammo a b), otherShips) = partitionShips ship ships
        (maybeLaserRayTheoretical, newAmmo) =
          if ammo > 0
            then
              (Just $ shootLaserWithOffset shipCoords dir Infinite (`location` space)
             , pred ammo)
            else
              (Nothing
             , ammo)

        ((remainingBalls, destroyedBalls), maybeLaserRay) =
           maybe
             ((balls,[]), Nothing)
             (\r -> fmap Just
                    $ computeActualLaserShot balls (\(Number (PosSpeed pos _) _) -> pos) r DestroyFirstObstacle)
               maybeLaserRayTheoretical
    putWorld $ World remainingBalls ((BattleShip ship shipPS newAmmo a b):otherShips) space rs d e

    let tps = systemTimePointToParticleSystemTimePoint t
    outerSpaceParticleSystems_ <-
      if null destroyedBalls
        then
          maybe (return []) (outerSpaceParticleSystems tps) maybeLaserRay
        else
          return []
    newSystems <- destroyedNumbersParticleSystems tps dir destroyedBalls
    laserSystems <- maybe (return []) (`laserParticleSystems` tps) maybeLaserRay
    addParticleSystems $ concat [newSystems, laserSystems, outerSpaceParticleSystems_]

    return destroyedBalls


outerSpaceParticleSystems :: (MonadState AppState m)
                          => Time Point ParticleSyst
                          -> LaserRay Actual
                          -> m [Prioritized ParticleSystem]
outerSpaceParticleSystems t ray@(LaserRay dir _ _) = do
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
                        cycleOuterColors1 $ quot _frame 4
                      else
                        cycleOuterColors2 $ quot _frame 4
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
                        cycleWallColors1 $ quot _frame 4
                      else
                        cycleWallColors2 $ quot _frame 4
                  (speedAttenuation, nRebounds) = (0.4, 5)
              outerSpaceParticleSystems' (WorldScope Wall) laserTarget
                   dir speedAttenuation nRebounds color char t

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
                     => LaserRay Actual
                     -> Time Point ParticleSyst
                     -> m [Prioritized ParticleSystem]
laserParticleSystems ray t =
  return $ catMaybes
    [fmap (Prioritized particleSystLaserPriority)
    $ laserShot ray cycleLaserColors t]


checkTargetAndAmmo :: Int
                   -- ^ Remaining ammo
                   -> Int
                   -- ^ The current sum of all shot 'Numbers'
                   -> Int
                   -- ^ The 'Level' 's target number.
                   -> Time Point System
                   -- ^ The current time
                   -> Maybe LevelFinished
checkTargetAndAmmo ammo sumNumbers target t =
    maybe Nothing (\stop -> Just $ LevelFinished stop t InfoMessage) allChecks
  where
    allChecks = checkSum <|> checkAmmo

    checkSum = case compare sumNumbers target of
      LT -> Nothing
      EQ -> Just Won
      GT -> Just $ Lost $ pack $ show sumNumbers ++ " is bigger than " ++ show target
    checkAmmo
      | ammo <= 0 = Just $ Lost $ pack "no ammo left"
      | otherwise = Nothing
