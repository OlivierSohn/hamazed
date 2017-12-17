{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Various animations of different kinds. Some are chained.

module Animation
    (
    -- * Circle / polygon expanding then shrinking
      animatedNumber
    -- * Gravity free-fall
    , gravityAnimation
    -- * Laser shoot
    , simpleLaser
    -- * Explosions
    , simpleExplosion
    -- * Explosions + fragmentation
    , explosion
    , explosion1
    , explosionGravity
    -- * Chained
    , gravityAnimationThenSimpleExplosion
    , quantitativeExplosionThenSimpleExplosion
    -- * Reexports
    , module Animation.Types
    , MonadIO
    , MonadReader
    ) where


import           Imajuscule.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Animation.Design.Animator
import           Animation.Design.Apply
import           Animation.Design.Chain
import           Animation.Design.Geo
import           Animation.Design.RenderUpdate
import           Animation.Types

import           Animation.Color
import           Draw
import           Geo.Continuous

import           Game.World.Laser.Types

import           Timing

-- | A laser ray animation
{-# INLINABLE simpleLaser #-}
simpleLaser :: (Draw e, MonadReader e m, MonadIO m)
            => LaserRay Actual
            -> AnimatedPoints
            -> Maybe KeyTime
            -> AnimationUpdate m
            -> (Coords -> Location)
            -> Coords
            -> m (Maybe (AnimationUpdate m))
simpleLaser seg =
  renderAndUpdate' (mkAnimator simpleLaserPure simpleLaser seg)

-- | An animation chaining two circular explosions, the first explosion
-- can be configured in number of points, the second is fixed to 4*8.
{-# INLINABLE quantitativeExplosionThenSimpleExplosion #-}
quantitativeExplosionThenSimpleExplosion :: (Draw e, MonadReader e m, MonadIO m)
                                         => Int
                                         -- ^ Number of points of the first circular explosion.
                                         -> AnimatedPoints
                                         -> Maybe KeyTime
                                         -> AnimationUpdate m
                                         -> (Coords -> Location)
                                         -> Coords
                                         -> m (Maybe (AnimationUpdate m))
quantitativeExplosionThenSimpleExplosion number = renderAndUpdate fPure f colorFromFrame
  where
    fPure = chainOnCollision (quantitativeExplosionPure number) (simpleExplosionPure 8)
    f = quantitativeExplosionThenSimpleExplosion number


-- | An animation chaining a gravity-based free-fall, with an initial velocity passed
-- as argument, and a circular explosion of 4*8 points.
{-# INLINABLE gravityAnimationThenSimpleExplosion #-}
gravityAnimationThenSimpleExplosion :: (Draw e, MonadReader e m, MonadIO m)
                                    => Vec2
                                    -- ^ Initial speed
                                    -> AnimatedPoints
                                    -> Maybe KeyTime
                                    -> AnimationUpdate m
                                    -> (Coords -> Location)
                                    -> Coords
                                    -> m (Maybe (AnimationUpdate m))
gravityAnimationThenSimpleExplosion initialSpeed = renderAndUpdate fPure f colorFromFrame
  where
    fPure = chainOnCollision (gravityFall initialSpeed) (simpleExplosionPure 8)
    f = gravityAnimationThenSimpleExplosion initialSpeed

-- | A gravity-based free-falling animation.
{-# INLINABLE gravityAnimation #-}
gravityAnimation :: (Draw e, MonadReader e m, MonadIO m)
                 => Vec2
                 -> AnimatedPoints
                 -> Maybe KeyTime
                 -> AnimationUpdate m
                 -> (Coords -> Location)
                 -> Coords
                 -> m (Maybe (AnimationUpdate m))
gravityAnimation initialSpeed = renderAndUpdate fPure f colorFromFrame
  where
    fPure = applyAnimation (gravityFall initialSpeed)
    f = gravityAnimation initialSpeed

-- | An animation where a geometric figure expands and then shrinks
{-# INLINABLE animatedNumber #-}
animatedNumber :: (Draw e, MonadReader e m, MonadIO m)
               => Int
               -- ^ If 1, the geometric figure is a circle, else if >1, a polygon
               -> AnimatedPoints
               -> Maybe KeyTime
               -> AnimationUpdate m
               -> (Coords -> Location)
               -> Coords
               -> m (Maybe (AnimationUpdate m))
animatedNumber n =
  renderAndUpdate' (mkAnimator animateNumberPure animatedNumber n)

-- | A circular explosion configurable in number of points
{-# INLINABLE simpleExplosion #-}
simpleExplosion :: (Draw e, MonadReader e m, MonadIO m)
                => Int
                -- ^ Number of points
                -> AnimatedPoints
                -> Maybe KeyTime
                -> AnimationUpdate m
                -> (Coords -> Location)
                -> Coords
                -> m (Maybe (AnimationUpdate m))
simpleExplosion resolution = renderAndUpdate fPure f colorFromFrame
  where
    fPure = applyAnimation (simpleExplosionPure resolution)
    f = simpleExplosion resolution

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts, and the parts explode when they hit a wall.
{-# INLINABLE explosionGravity #-}
explosionGravity :: (Draw e, MonadReader e m, MonadIO m)
                 => Vec2
                 -> Coords
                 -> [Maybe KeyTime -> AnimationUpdate m -> (Coords -> Location) -> Coords -> m (Maybe (AnimationUpdate m))]
explosionGravity speed pos =
  map (`explosionGravity1` pos) $ variations speed

{-# INLINABLE explosionGravity1 #-}
explosionGravity1 :: (Draw e, MonadReader e m, MonadIO m)
                  => Vec2
                  -> Coords
                  -> (Maybe KeyTime -> AnimationUpdate m -> (Coords -> Location) -> Coords -> m (Maybe (AnimationUpdate m)))
explosionGravity1 speed pos =
  gravityAnimation speed (mkAnimatedPoints pos (ReboundAnd Stop))

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts.
{-# INLINABLE explosion #-}
explosion :: (Draw e, MonadReader e m, MonadIO m)
          => Vec2
          -> Coords
          -> [Maybe KeyTime -> AnimationUpdate m -> (Coords -> Location) -> Coords -> m (Maybe (AnimationUpdate m))]
explosion speed pos =
  map (`explosion1` pos) $ variations speed

-- | Given an input speed, computes four slightly different input speeds
variations :: Vec2 -> [Vec2]
variations sp =
  map (sumVec2d sp) [ Vec2 0.3     (-0.4)
                    , Vec2 (-0.55) (-0.29)
                    , Vec2 (-0.1)  0.9
                    , Vec2 1.2     0.2]

{-# INLINABLE explosion1 #-}
explosion1 :: (Draw e, MonadReader e m, MonadIO m)
           => Vec2
           -> Coords
           -> (Maybe KeyTime -> AnimationUpdate m -> (Coords -> Location) -> Coords -> m (Maybe (AnimationUpdate m)))
explosion1 speed pos =
  gravityAnimationThenSimpleExplosion speed (mkAnimatedPoints pos (ReboundAnd $ ReboundAnd Stop))
