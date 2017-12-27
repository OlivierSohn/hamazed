{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.World
    (
      -- * Parameters
      {-| When the game starts, the player can chose :

      * 'WorldShape' : square or rectangular 'World' where the width is twice the height
      * 'WallDistribution' : Should the 'World' have walls, and what kind of walls.
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
    , isLevelFinished
    -- * World
    {- | A 'World' brings together:

    * game elements : 'Space', 'BattleShip' and 'Number's,
    * rendering elements: 'BoundedAnimation's,
    * terminal-awareness : 'EmbeddedWorld'
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
    , updateWorld
    -- ** Render World
    , renderWorld
    -- ** World utilities
    -- | When a player 'Event' occurs (like a laser shot), 'eventAction' returns
    -- the effect that this 'Event' would have on the 'World'.
    , eventAction
    -- * EmbeddedWorld
    -- | 'EmbeddedWorld' allows to place the game in the center of the terminal.
    , mkEmbeddedWorld
    , EmbeddedWorld(..)
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

      'scopedLocation' prevides more options with the use of 'Boundaries' to
      defines the collision detection scopes.
       -}
    , location
    , scopedLocation
    , Boundaries(..)
      -- ** Collision detection utilities
    , createRandomNonCollidingPosSpeed
      -- ** Rendering
    , renderSpace
      -- * Movable items
      -- | A Movable item's 'PosSpeed' is updated using 'updateMovableItem'
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
    -- 'BoundedAnimation' explosions.
    --
    -- 'Number's never change speed, except when they rebound on 'Wall's, of course.
    , Number(..)
    -- * BoundedAnimation
    -- | 'BoundedAnimation' allows to specify in which environment an 'Animation'
    -- runs : in the world, in the terminal, in both (see 'Boundaries')
    , BoundedAnimation(..)
    , earliestAnimationDeadline
    -- * UI
    {- | UI elements around the 'World' are:

    * a 'RectFrame' created by 'mkFrameSpec' to visually delimit the 'World'
    * 'ColorString' information, placed around the 'RectFrame':

        * Up: 'Level' target
        * Left: remaining ammunitions / shot 'Number's
        * Down: 'Level' number
    -}
    , mkFrameSpec
    -- ** Inter-level animations
    , module Imj.Game.Level.Animation
    -- * Secondary types
    , WallDistribution(..)
    , WorldShape(..)
    , LevelFinished(..)
    , GameStops(..)
    -- * Reexports
    , module Imj.Draw
    , ColorString
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Maybe( isNothing )
import           Data.Text( pack )

import           Imj.Animation.Design.Types
import           Imj.Draw
import           Imj.Game.Event
import           Imj.Game.Level.Types
import           Imj.Game.Level.Animation
import           Imj.Game.Parameters
import           Imj.Game.Timing
import           Imj.Game.World.Create
import           Imj.Game.World.Embedded
import           Imj.Game.World.Number
import           Imj.Game.World.Render
import           Imj.Game.World.Size
import           Imj.Game.World.Space
import           Imj.Game.World.Types
import           Imj.Geo.Discrete
import           Imj.Laser
import           Imj.Text.ColorString
import           Imj.Util

-- | Note that the position of the 'BattleShip' remains unchanged.
accelerateShip :: Direction -> BattleShip -> BattleShip
accelerateShip dir (BattleShip (PosSpeed pos speed) ba bb bc) =
  let newSpeed = translateInDir dir speed
  in BattleShip (PosSpeed pos newSpeed) ba bb bc

-- | Moves elements of game logic ('Number's, 'BattleShip').
--
-- Note that 'BoundedAnimation's are not updated.
updateWorld :: SystemTime
            -- ^ The current time
            -> World
            -> World
updateWorld curTime (World balls (BattleShip shipPosSpeed ammo safeTime _) size anims e) =
  let newSafeTime = case safeTime of
        (Just t) -> if curTime > t then Nothing else safeTime
        _        -> Nothing
      newBalls = map (\(Number ps n) -> Number (updateMovableItem size ps) n) balls
      newPosSpeed@(PosSpeed pos _) = updateMovableItem size shipPosSpeed
      collisions = getColliding pos newBalls
      newShip = BattleShip newPosSpeed ammo newSafeTime collisions
  in World newBalls newShip size anims e

-- | Returns the earliest 'BoundedAnimation' deadline.
earliestAnimationDeadline :: World -> Maybe KeyTime
earliestAnimationDeadline (World _ _ _ animations _) =
  earliestDeadline $ map (\(BoundedAnimation a _) -> a) animations

-- TODO use Number Live Number Dead
-- | Computes the effect of an 'Event' on the 'World'.
eventAction :: Event
            -- ^ The event whose action will be evaluated
            -> World
            -> ([Number], [Number], Maybe (LaserRay Actual), Int)
            -- ^ 'Number's still alive, 'Number's destroyed, maybe an actual laser ray, Ammo left.
eventAction
  event
  (World balls (BattleShip (PosSpeed shipCoords _) ammo safeTime collisions)
        space _ _)
 =
  let (maybeLaserRayTheoretical, newAmmo) =
       if ammo > 0 then case event of
         (Action Laser dir) ->
           (LaserRay dir <$> shootLaserWithOffset shipCoords dir Infinite (`location` space), pred ammo)
         _ ->
           (Nothing, ammo)
       else
         (Nothing, ammo)

      ((remainingBalls', destroyedBalls), maybeLaserRay) =
         maybe
           ((balls,[]), Nothing)
           (\r -> computeActualLaserShot balls (\(Number (PosSpeed pos _) _) -> pos) r DestroyFirstObstacle)
             maybeLaserRayTheoretical

      remainingBalls = case event of
         Timeout (Deadline _ MoveFlyingItems) ->
           if isNothing safeTime
             then
               filter (`notElem` collisions) remainingBalls'
             else
               remainingBalls'
         _ -> remainingBalls'
  in (remainingBalls, destroyedBalls, maybeLaserRay, newAmmo)


isLevelFinished :: World
                -> Int
                -- ^ The current sum of all shot 'Numbers'
                -> Int
                -- ^ The 'Level' 's target number.
                -> TimestampedEvent
                -- ^ The current event
                -> Maybe LevelFinished
isLevelFinished (World _ (BattleShip _ ammo safeTime collisions) _ _ _) sumNumbers target (TimestampedEvent lastEvent t) =
    maybe Nothing (\stop -> Just $ LevelFinished stop t InfoMessage) allChecks
  where
    allChecks = checkShipCollision <|> checkSum <|> checkAmmo

    checkShipCollision = case lastEvent of
      Timeout (Deadline _ MoveFlyingItems) ->
        maybe
          (case map (\(Number _ n) -> n) collisions of
            [] -> Nothing
            l  -> Just $ Lost $ "collision with " <> showListOrSingleton l)
          (const Nothing)
          safeTime
      _ -> Nothing -- this optimization is to not re-do the check when nothing has moved

    checkSum = case compare sumNumbers target of
      LT -> Nothing
      EQ -> Just Won
      GT -> Just $ Lost $ pack $ show sumNumbers ++ " is bigger than " ++ show target
    checkAmmo
      | ammo <= 0 = Just $ Lost $ pack "no ammo left"
      | otherwise = Nothing
