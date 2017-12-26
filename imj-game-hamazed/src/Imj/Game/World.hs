{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.World
    ( -- * Space
    {-| 'Space' describes the environment in which 'Number's and the 'BattleShip'
    live.

    It can be composed of 'Air', where 'BattleShip' and 'Number's are free to move, and of
    'Wall'.
    -}
      Space
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
      -- ** Rendering
    , renderSpace
      -- * Space Utilities
    , createRandomNonCollidingPosSpeed
    -- * EmbeddedWorld
    -- | 'EmbeddedWorld' allows to place the game in the center of the terminal.
    , mkEmbeddedWorld
    , EmbeddedWorld(..)
    -- * World
    {- | 'World' brings together:

    * game elements : 'Space', 'BattleShip' and 'Number's,
    * rendering elements: 'BoundedAnimation's,
    * terminal-awareness : 'EmbeddedWorld'
    -}
    , World(..)
    -- ** World size for a given Level
      {-| The higher the level, the smaller the 'World', and the more 'Number's are
      flying around!

      'worldSizeFromLevel' gives you the size of the world based on
      the level number and the 'WorldShape'.

      Two 'WorldShape's are currently supported : square and rectangular where the
      width is twice the height. -}
    , WorldShape(..)
    , worldSizeFromLevel
    -- ** Create the world
    , mkWorld
    , WallDistribution(..)
    -- ** Update World
    , updateWorld
    , eventAction
    -- ** Render World
    , renderWorld
    -- * BattleShip
    , BattleShip(..)
    -- ** Move BattleShip
    -- | The 'BattleShip' is controlled in (discrete) acceleration by the player
    -- using the keyboard.
    , accelerateShip
    -- * Number
    -- | 'Number's can be shot by the 'BattleShip'. In that case, their numeric value
    -- is added to the current sum, and if it matches the level's target number, the player has
    -- finished the level.
    --
    -- Number can collide with the 'BattleShip', hence triggering colorfull
    -- 'BoundedAnimation' explosions.
    --
    -- 'Number's never change speed, except when they rebound on 'Wall's, of course.
    , Number(..)
    -- * UI / Animations
    -- ** BoundedAnimation
    -- | 'BoundedAnimation' allows to specify in which environment an 'Animation'
    -- runs : in the world, in the terminal, in both (see 'Boundaries')
    , BoundedAnimation(..)
    , earliestAnimationDeadline
    -- ** World Animated UI
    {- | UI elements around the 'World' are:

    * a 'RectFrame'
    * Colored textual informations (aligned the 4 sides of the 'RectFrame')

    'WorldEvolutions' makes these elements animate during level changes
    to provide a smooth visual transformation.
    -}
    , mkFrameSpec
    , WorldEvolutions(..)
    , WorldAnimation(..)
    , renderWorldAnimation
    , isFinished
    -- * Reexports
    , module Imj.Draw
    ) where

import           Imj.Prelude

import           Control.Monad.Reader(when)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Char( intToDigit )
import           Data.Maybe( isNothing, isJust )

import           Imj.Animation.Design.Types
import           Imj.Draw
import           Imj.Geo.Discrete.Bresenham
import           Imj.Geo.Discrete
import           Imj.Game.Color
import           Imj.Game.Event
import           Imj.Game.World.Embedded
import           Imj.Game.World.Evolution
import           Imj.Game.World.Laser
import           Imj.Game.World.Number
import           Imj.Game.World.Ship
import           Imj.Game.World.Size
import           Imj.Game.World.Space
import           Imj.Game.World.Types
import           Imj.Physics.Discrete.Collision
import           Imj.Timing


data WorldShape = Square
                -- ^ Width = Height
                | Rectangle2x1
                -- ^ Width = 2 * Height

worldSizeFromLevel :: Int
                   -- ^ 'Level' number
                   -> WorldShape -> Size
worldSizeFromLevel level shape =
  let h = heightFromLevel level
      -- we need even world dimensions to ease level construction
      w = fromIntegral $ assert (even h) h * case shape of
        Square       -> 1
        Rectangle2x1 -> 2
  in Size h w

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
updateWorld curTime (World balls changePos (BattleShip shipPosSpeed ammo safeTime _) size anims e) =
  let newSafeTime = case safeTime of
        (Just t) -> if curTime > t then Nothing else safeTime
        _        -> Nothing
      newBalls = map (\(Number ps n) -> Number (changePos size ps) n) balls
      newPosSpeed@(PosSpeed pos _) = changePos size shipPosSpeed
      collisions = getColliding pos newBalls
      newShip = BattleShip newPosSpeed ammo newSafeTime collisions
  in World newBalls changePos newShip size anims e

ballMotion :: Space -> PosSpeed -> PosSpeed
ballMotion space ps@(PosSpeed pos _) =
  let (newPs@(PosSpeed newPos _), collision) =
        mirrorSpeedAndMoveToPrecollisionIfNeeded (`location` space) ps
  in  case collision of
        PreCollision ->
          if pos /= newPos
            then
              newPs
            else
              -- Precollision position is the same as the previous position, we try to move
              doBallMotionUntilCollision space newPs
        NoCollision  -> doBallMotion newPs

doBallMotion :: PosSpeed -> PosSpeed
doBallMotion (PosSpeed pos speed) =
  PosSpeed (sumCoords pos speed) speed

-- | Changes the position until a collision is found.
--   Doesn't change the speed
doBallMotionUntilCollision :: Space -> PosSpeed -> PosSpeed
doBallMotionUntilCollision space (PosSpeed pos speed) =
  let trajectory = bresenham $ mkSegment pos $ sumCoords pos speed
      newPos = maybe (last trajectory) snd $ firstCollision (`location` space) trajectory
  in PosSpeed newPos speed

-- | Returns the earliest 'BoundedAnimation' deadline.
earliestAnimationDeadline :: World -> Maybe KeyTime
earliestAnimationDeadline (World _ _ _ _ animations _) =
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
  (World balls _ (BattleShip (PosSpeed shipCoords _) ammo safeTime collisions)
        space _ _)
 =
  let (maybeLaserRayTheoretical, newAmmo) =
       if ammo > 0 then case event of
         (Action Laser dir) ->
           (LaserRay dir <$> shootLaserFromShip shipCoords dir Infinite (`location` space), pred ammo)
         _ ->
           (Nothing, ammo)
       else
         (Nothing, ammo)

      ((remainingBalls', destroyedBalls), maybeLaserRay) =
         maybe
           ((balls,[]), Nothing)
           (survivingNumbers balls RayDestroysFirst)
             maybeLaserRayTheoretical

      remainingBalls = case event of
         Timeout GameDeadline _ ->
           if isNothing safeTime
             then
               filter (`notElem` collisions) remainingBalls'
             else
               remainingBalls'
         _ -> remainingBalls'
  in (remainingBalls, destroyedBalls, maybeLaserRay, newAmmo)

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

mkWorld :: (MonadIO m)
        => EmbeddedWorld
        -- ^ Tells where to draw the 'World' from
        -> Size
        -- ^ The dimensions
        -> WallDistribution
        -- ^ How the 'Wall's should be constructed
        -> [Int]
        -- ^ The numbers for which we will create 'Number's.
        -> Int
        -- ^ Ammunition : how many laser shots are available.
        -> m World
mkWorld e s walltype nums ammo = do
  space <- case walltype of
    None          -> return $ mkEmptySpace s
    Deterministic -> return $ mkDeterministicallyFilledSpace s
    Random rParams    -> liftIO $ mkRandomlyFilledSpace rParams s
  t <- liftIO getSystemTime
  balls <- mapM (createRandomNumber space) nums
  ship@(PosSpeed pos _) <- liftIO $ createShipPos space balls
  return $ World balls ballMotion (BattleShip ship ammo (Just $ addSystemTime 5 t) (getColliding pos balls)) space [] e

createRandomNumber :: (MonadIO m)
                   => Space
                   -> Int
                   -> m Number
createRandomNumber space i = do
  ps <- liftIO $ createRandomNonCollidingPosSpeed space
  return $ Number ps i


{-# INLINABLE renderWorld #-}
renderWorld :: (Draw e, MonadReader e m, MonadIO m)
            => World
            -> m ()
renderWorld
  (World balls _ (BattleShip (PosSpeed shipCoords _) _ safeTime collisions)
         space _ (EmbeddedWorld _ upperLeft))  = do
  -- render numbers, including the ones that will be destroyed, if any
  let s = translateInDir Down $ translateInDir RIGHT upperLeft
  mapM_ (\b -> renderNumber b space s) balls
  when ((null collisions || isJust safeTime) && (InsideWorld == location shipCoords space)) $ do
    let colors =
          if isNothing safeTime
            then
              shipColors
            else
              shipColorsSafe
    drawChar '+' (sumCoords shipCoords s) colors


{-# INLINABLE renderNumber #-}
renderNumber :: (Draw e, MonadReader e m, MonadIO m)
             => Number
             -> Space
             -> Coords
             -> m ()
renderNumber (Number (PosSpeed pos _) i) space b =
  when (location pos space == InsideWorld) $
    drawChar (intToDigit i) (sumCoords pos b) (numberColor i)


{-# INLINABLE renderWorldAnimation #-}
renderWorldAnimation :: (Draw e, MonadReader e m, MonadIO m)
                     => WorldAnimation
                     -> m ()
renderWorldAnimation (WorldAnimation evolutions _ (Iteration _ frame)) =
  renderEvolutions evolutions frame
