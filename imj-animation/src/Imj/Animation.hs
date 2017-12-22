{-# LANGUAGE NoImplicitPrelude #-}

{- | This module exports functions to create explosive, free-fall, fragments animations,
and several compositions of these animations. A laser animation is also available.

These functions contain boilerplate code to abstract away the use of:

* 'mkAnimatedPoints', so that the user of
the module doesn't have to know whether animations are composed and how they
should interact with their environment.
* 'mkAnimationUpdate', so that the user of the module doesn't have to think
about how to handle the initial frame.
-}
-- TODO also abstract away the use of mkAnimationUpdate?

module Imj.Animation
    (
    -- * Explosion
      simpleExplosion
    -- * Composed explosions
    , quantitativeExplosionThenSimpleExplosion
    -- * Free fall
    , freeFall
    -- ** Followed by an explosion
    , freeFallThenExplode
    -- * Free falling fragments
    , fragmentsFreeFall
    -- ** Followed by an explosion
    , fragmentsFreeFallThenExplode
    -- * A polygon (or circle) explodes then implodes
    , animatedPolygon
    -- * Laser ray
    , laserAnimation
    -- * Reexports
    -- ** Animation types
    , module Imj.Animation.Types
    -- ** Other
    , MonadIO
    , MonadReader
    ) where


import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Animation.Color
import           Imj.Animation.Design.Animator
import           Imj.Animation.Design.Apply
import           Imj.Animation.Design.Chain
import           Imj.Animation.Design.Geo
import           Imj.Animation.Design.RenderUpdate
import           Imj.Animation.Types
import           Imj.Draw
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Laser.Types
import           Imj.Timing

-- | A laser ray animation
{-# INLINABLE laserAnimation #-}
laserAnimation :: (Draw e, MonadReader e m, MonadIO m)
               => LaserRay Actual
               -- ^ The laser ray
               -> KeyTime
               -- ^ 'KeyTime' of the game event that started this animation
               -> AnimationUpdate m
laserAnimation ray@(LaserRay _ (Ray seg)) k =
  let collisionFree = fst $ extremities seg -- this needs to be collision-free
      f = laserAnimation' ray collisionFree
  in mkAnimationUpdate f k WithZero (Speed 1) Nothing

-- | A laser ray animation
{-# INLINABLE laserAnimation' #-}
laserAnimation' :: (Draw e, MonadReader e m, MonadIO m)
            => LaserRay Actual
            -- ^ The laser ray
            -> Coords
            -- ^ A collision-free point (for example the first point of the laser ray)
            -> Maybe KeyTime
            -> AnimationUpdate m
            -> (Coords -> InteractionResult)
            -> Coords
            -> m (Maybe (AnimationUpdate m))
laserAnimation' seg ref =
  laserAnimation'' seg (mkAnimatedPoints ref DontInteract)

{-# INLINABLE laserAnimation'' #-}
laserAnimation'' :: (Draw e, MonadReader e m, MonadIO m)
                 => LaserRay Actual
                 -- ^ The laser ray
                 -> AnimatedPoints
                 -> Maybe KeyTime
                 -> AnimationUpdate m
                 -> (Coords -> InteractionResult)
                 -> Coords
                 -> m (Maybe (AnimationUpdate m))
laserAnimation'' seg =
  renderAndUpdate' (mkAnimator simpleLaserPure laserAnimation'' seg)

{-# INLINABLE quantitativeExplosionThenSimpleExplosion #-}
quantitativeExplosionThenSimpleExplosion :: (Draw e, MonadReader e m, MonadIO m)
                                         => Int
                                         -- ^ Number of points in the first explosion
                                         -> Coords
                                         -- ^ Center of the first explosion
                                         -> KeyTime
                                         -- ^ 'KeyTime' of the game event that started this animation
                                         -> Speed
                                         -- ^ Animation speed
                                         -> Char
                                         -- ^ Character used when drawing the animation.
                                         -> AnimationUpdate m
quantitativeExplosionThenSimpleExplosion resolution ref keyTime animSpeed char =
  let f = quantitativeExplosionThenSimpleExplosion' resolution ref
  in mkAnimationUpdate f keyTime SkipZero animSpeed (Just char)

-- | An animation chaining two circular explosions, the first explosion
-- can be configured in number of points, the second has 4*8=32 points.
{-# INLINABLE quantitativeExplosionThenSimpleExplosion' #-}
quantitativeExplosionThenSimpleExplosion' :: (Draw e, MonadReader e m, MonadIO m)
                                         => Int
                                         -- ^ Number of points of the first circular explosion.
                                         -> Coords
                                         -- ^ Center of the first explosion
                                         -> Maybe KeyTime
                                         -> AnimationUpdate m
                                         -> (Coords -> InteractionResult)
                                         -> Coords
                                         -> m (Maybe (AnimationUpdate m))
quantitativeExplosionThenSimpleExplosion' number center =
  let points = mkAnimatedPoints center (Interact $ Interact Stop)
  in quantitativeExplosionThenSimpleExplosion'' number points

-- | An animation chaining two circular explosions, the first explosion
-- can be configured in number of points, the second has 4*8=32 points.
{-# INLINABLE quantitativeExplosionThenSimpleExplosion'' #-}
quantitativeExplosionThenSimpleExplosion'' :: (Draw e, MonadReader e m, MonadIO m)
                                          => Int
                                          -- ^ Number of points of the first circular explosion.
                                          -> AnimatedPoints
                                          -> Maybe KeyTime
                                          -> AnimationUpdate m
                                          -> (Coords -> InteractionResult)
                                          -> Coords
                                          -> m (Maybe (AnimationUpdate m))
quantitativeExplosionThenSimpleExplosion'' number =
  renderAndUpdate fPure f colorFromFrame
 where
  fPure = chainOnCollision (quantitativeExplosionPure number) (simpleExplosionPure 8)
  f = quantitativeExplosionThenSimpleExplosion'' number

animatedPolygon :: (Draw e, MonadReader e m, MonadIO m)
                => Int
                -- ^ If n==1, the geometric figure is a circle, else if n>1, a n-sided polygon
                -> Coords
                -- ^ Center of the polygon (or circle)
                -> KeyTime
                -- ^ 'KeyTime' of the game event that started this animation
                -> Speed
                -- ^ Animation speed
                -> Char
                -- ^ Character used when drawing the animation.
                -> AnimationUpdate m
animatedPolygon n pos keyTime animSpeed char =
  let f = animatedPolygon' n pos
  in mkAnimationUpdate f keyTime SkipZero animSpeed (Just char)

-- | An animation where a geometric figure (polygon or circle) expands and then shrinks
{-# INLINABLE animatedPolygon' #-}
animatedPolygon' :: (Draw e, MonadReader e m, MonadIO m)
                 => Int
                 -- ^ If n==1, the geometric figure is a circle, else if n>1, a n-sided polygon
                 -> Coords
                 -- ^ The center of the geometric figure
                 -> Maybe KeyTime
                 -> AnimationUpdate m
                 -> (Coords -> InteractionResult)
                 -> Coords
                 -> m (Maybe (AnimationUpdate m))
animatedPolygon' n center =
  animatedPolygon'' n (mkAnimatedPoints center DontInteract)


-- | An animation where a geometric figure (polygon or circle) expands and then shrinks
{-# INLINABLE animatedPolygon'' #-}
animatedPolygon'' :: (Draw e, MonadReader e m, MonadIO m)
                  => Int
                  -- ^ If n==1, the geometric figure is a circle, else if n>1, a n-sided polygon
                  -> AnimatedPoints
                  -> Maybe KeyTime
                  -> AnimationUpdate m
                  -> (Coords -> InteractionResult)
                  -> Coords
                  -> m (Maybe (AnimationUpdate m))
animatedPolygon'' n =
  renderAndUpdate' (mkAnimator animateNumberPure animatedPolygon'' n)


-- | A circular explosion configurable in number of points
{-# INLINABLE simpleExplosion #-}
simpleExplosion :: (Draw e, MonadReader e m, MonadIO m)
                 => Int
                 -- ^ Number of points in the explosion
                 -> Coords
                 -- ^ Center of the explosion
                  -> KeyTime
                  -- ^ 'KeyTime' of the game event that started this animation
                  -> Speed
                  -- ^ Animation speed
                  -> Char
                  -- ^ Character used when drawing the animation.
                 -> AnimationUpdate m
simpleExplosion resolution ref keyTime animSpeed char =
  let f = simpleExplosion' resolution ref
  in mkAnimationUpdate f keyTime SkipZero animSpeed (Just char)

-- | A circular explosion configurable in number of points
{-# INLINABLE simpleExplosion' #-}
simpleExplosion' :: (Draw e, MonadReader e m, MonadIO m)
                 => Int
                 -- ^ Number of points in the explosion
                 -> Coords
                 -- ^ Center of the explosion
                 -> Maybe KeyTime
                 -> AnimationUpdate m
                 -> (Coords -> InteractionResult)
                 -> Coords
                 -> m (Maybe (AnimationUpdate m))
simpleExplosion' resolution ref =
  simpleExplosion'' resolution (mkAnimatedPoints ref (Interact Stop))


-- | A circular explosion configurable in number of points
{-# INLINABLE simpleExplosion'' #-}
simpleExplosion'' :: (Draw e, MonadReader e m, MonadIO m)
                  => Int
                  -- ^ Number of points in the explosion
                  -> AnimatedPoints
                  -> Maybe KeyTime
                  -> AnimationUpdate m
                  -> (Coords -> InteractionResult)
                  -> Coords
                  -> m (Maybe (AnimationUpdate m))
simpleExplosion'' resolution =
  renderAndUpdate fPure f colorFromFrame
 where
  fPure = applyAnimation (simpleExplosionPure resolution)
  f = simpleExplosion'' resolution

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts, and the parts explode when they hit a wall.
{-# INLINABLE fragmentsFreeFall #-}
fragmentsFreeFall :: (Draw e, MonadReader e m, MonadIO m)
                  => Vec2
                  -- ^ Initial speed
                  -> Coords
                  -- ^ Initial position
                  -> KeyTime
                  -- ^ 'KeyTime' of the game event that started this animation
                  -> Speed
                  -- ^ Animation speed
                  -> Char
                  -- ^ Character used when drawing the animation.
                  -> [AnimationUpdate m]
fragmentsFreeFall speed pos keyTime animSpeed char =
  let freeFalls = fragmentsFreeFall' speed pos
  in map (\f -> mkAnimationUpdate f keyTime WithZero animSpeed (Just char)) freeFalls

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts, and the parts explode when they hit a wall.
{-# INLINABLE fragmentsFreeFall' #-}
fragmentsFreeFall' :: (Draw e, MonadReader e m, MonadIO m)
                   => Vec2
                   -- ^ Initial speed
                   -> Coords
                   -- ^ Initial position
                   -> [Maybe KeyTime -> AnimationUpdate m -> (Coords -> InteractionResult) -> Coords -> m (Maybe (AnimationUpdate m))]
fragmentsFreeFall' speed pos =
  map (`freeFall'` pos) $ variations speed


-- | A gravity-based free-falling animation.
freeFall :: (Draw e, MonadReader e m, MonadIO m)
         => Vec2
         -- ^ Initial speed
         -> Coords
         -- ^ Initial position
         -> KeyTime
         -- ^ 'KeyTime' of the game event that started this animation
         -> Speed
         -- ^ Animation speed
         -> Char
         -- ^ Character used when drawing the animation.
         -> AnimationUpdate m
freeFall speed pos keyTime animSpeed char =
  let f = freeFall' speed pos
  in mkAnimationUpdate f keyTime SkipZero animSpeed (Just char)

-- | A gravity-based free-falling animation.
{-# INLINABLE freeFall' #-}
freeFall' :: (Draw e, MonadReader e m, MonadIO m)
                  => Vec2
                  -- ^ Initial speed
                  -> Coords
                 -- ^ Initial position
                  -> (Maybe KeyTime -> AnimationUpdate m -> (Coords -> InteractionResult) -> Coords -> m (Maybe (AnimationUpdate m)))
freeFall' speed pos =
  let points = mkAnimatedPoints pos (Interact Stop)
  in freeFall'' speed points

-- | A gravity-based free-falling animation.
{-# INLINABLE freeFall'' #-}
freeFall'' :: (Draw e, MonadReader e m, MonadIO m)
                 => Vec2
                 -- ^ Initial speed
                 -> AnimatedPoints
                 -> Maybe KeyTime
                 -> AnimationUpdate m
                 -> (Coords -> InteractionResult)
                 -> Coords
                 -> m (Maybe (AnimationUpdate m))
freeFall'' initialSpeed =
  renderAndUpdate fPure f colorFromFrame
 where
  fPure = applyAnimation (gravityFall initialSpeed)
  f = freeFall'' initialSpeed

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts free-falling and then exploding.
{-# INLINABLE fragmentsFreeFallThenExplode #-}
fragmentsFreeFallThenExplode :: (Draw e, MonadReader e m, MonadIO m)
          => Vec2
          -- ^ Initial speed
          -> Coords
          -- ^ Initial position
          -> KeyTime
          -- ^ 'KeyTime' of the game event that started this animation
          -> Speed
          -- ^ Animation speed
          -> Char
          -- ^ Character used when drawing the animation.
          -> [AnimationUpdate m]
fragmentsFreeFallThenExplode speed pos k s c =
  map (\sp -> freeFallThenExplode sp pos k s c) $ variations speed

-- | Given an input speed, computes four slightly different input speeds
variations :: Vec2 -> [Vec2]
variations sp =
  map (sumVec2d sp) [ Vec2 0.3     (-0.4)
                    , Vec2 (-0.55) (-0.29)
                    , Vec2 (-0.1)  0.9
                    , Vec2 1.2     0.2]

-- | An animation chaining a gravity-based free-fall and a circular explosion of 4*8 points.
{-# INLINABLE freeFallThenExplode #-}
freeFallThenExplode :: (Draw e, MonadReader e m, MonadIO m)
                    => Vec2
                    -- ^ Initial speed
                    -> Coords
                    -- ^ Initial position
                    -> KeyTime
                    -- ^ 'KeyTime' of the game event that started this animation
                    -> Speed
                    -- ^ Animation speed
                    -> Char
                    -- ^ Character used when drawing the animation.
                    -> AnimationUpdate m
freeFallThenExplode speed pos keyTime animSpeed char =
  let f = freeFallThenExplode' speed pos
  in mkAnimationUpdate f keyTime SkipZero animSpeed (Just char)

-- | An animation chaining a gravity-based free-fall and a circular explosion of 4*8 points.
{-# INLINABLE freeFallThenExplode' #-}
freeFallThenExplode' :: (Draw e, MonadReader e m, MonadIO m)
                     => Vec2
                     -- ^ Initial speed
                     -> Coords
                     -- ^ Initial position
                     -> (Maybe KeyTime -> AnimationUpdate m -> (Coords -> InteractionResult) -> Coords -> m (Maybe (AnimationUpdate m)))
freeFallThenExplode' speed pos =
  let points = mkAnimatedPoints pos (Interact $ Interact Stop)
  in freeFallThenExplode'' speed points

-- | An animation chaining a gravity-based free-fall and a circular explosion of 4*8 points.
{-# INLINABLE freeFallThenExplode'' #-}
freeFallThenExplode'' :: (Draw e, MonadReader e m, MonadIO m)
                      => Vec2
                      -- ^ Initial speed
                      -> AnimatedPoints
                      -> Maybe KeyTime
                      -> AnimationUpdate m
                      -> (Coords -> InteractionResult)
                      -> Coords
                      -> m (Maybe (AnimationUpdate m))
freeFallThenExplode'' initialSpeed =
  renderAndUpdate fPure f colorFromFrame
 where
  fPure = chainOnCollision (gravityFall initialSpeed) (simpleExplosionPure 8)
  f = freeFallThenExplode'' initialSpeed
