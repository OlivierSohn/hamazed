{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World
    (
      -- * Parameters
      {-| When the game starts, the player can chose 'World' parameters:

      * 'WorldShape' : square or rectangular 'World' where the width is twice the height
      * 'WallDistribution' : Should the 'World' have walls, and what kind of walls.
      * 'ViewMode' : Should the view be centered on the 'BattleShip' or not.
       -}
      getGameParameters
    , GameParameters(..)
      -- * Level
      {-| There are 12 levels in Hamazed, numbered from 1 to 12.
      -}
    , Level(..)
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
    -- ** Create the world
      {-|
      The 'World' size decreases with increasing 'Level' numbers.

      'worldSizeFromLevel' gives the 'Size' of the 'World' based on
      the 'Level' number and the 'WorldShape':
      -}
    , worldSizeFromLevel
      {-|
      Once we have the 'Size' of the 'World', we can create it using 'mkWorld':
      -}
    , mkWorld
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
    -- * InTerminal
    -- | 'InTerminal' allows to place the game in the center of the terminal.
    , mkInTerminal
    , InTerminal(..)
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
    -- *** Accelerate BattleShip
    -- | The 'BattleShip' is controlled in (discrete) acceleration by the player
    -- using the keyboard.
    , accelerateShip
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
    , GameStops(..)
    -- * Reexports
    , module Imj.Graphics.Render
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Text( pack )

import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Parameters
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Draw
import           Imj.Game.Hamazed.World.InTerminal
import           Imj.Game.Hamazed.World.Number
import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Discrete
import           Imj.Graphics.Render
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.RectContainer

-- | Note that the position of the 'BattleShip' remains unchanged.
accelerateShip :: Direction -> BattleShip -> BattleShip
accelerateShip dir (BattleShip (PosSpeed pos speed) ba bb bc) =
  let newSpeed = translateInDir dir speed
  in BattleShip (PosSpeed pos newSpeed) ba bb bc

-- | Moves elements of game logic ('Number's, 'BattleShip').
--
-- Note that 'ParticleSystem's are not updated.
moveWorld :: KeyTime
          -- ^ The current time
          -> World
          -> World
moveWorld (KeyTime curTime) (World balls (BattleShip shipPosSpeed ammo safeTime _) size anims e) =
  let newSafeTime = case safeTime of
        (Just t) -> if curTime > t
                      then
                        Nothing
                      else
                        safeTime
        _        -> Nothing
      newBalls = map (\(Number ps n) -> Number (updateMovableItem size ps) n) balls
      newPosSpeed@(PosSpeed pos _) = updateMovableItem size shipPosSpeed
      collisions = getColliding pos newBalls
      newShip = BattleShip newPosSpeed ammo newSafeTime collisions
  in World newBalls newShip size anims e

-- TODO use Number Live Number Dead
-- | Computes the effect of an laser shot on the 'World'.
laserEventAction :: Direction
                 -- ^ The direction of the laser shot
                 -> World
                 -> ([Number], [Number], Maybe (LaserRay Actual), Int)
                 -- ^ 'Number's still alive, 'Number's destroyed, maybe an actual laser ray, Ammo left.
laserEventAction dir (World balls (BattleShip (PosSpeed shipCoords _) ammo _ _) space _ _) =
  let (maybeLaserRayTheoretical, newAmmo) =
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
           (\r -> let (a,laserRay) = computeActualLaserShot balls (\(Number (PosSpeed pos _) _) -> pos) r DestroyFirstObstacle
                  in (a, Just laserRay))
             maybeLaserRayTheoretical

  in (remainingBalls, destroyedBalls, maybeLaserRay, newAmmo)


checkTargetAndAmmo :: Int
                   -- ^ Remaining ammo
                   -> Int
                   -- ^ The current sum of all shot 'Numbers'
                   -> Int
                   -- ^ The 'Level' 's target number.
                   -> SystemTime
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
