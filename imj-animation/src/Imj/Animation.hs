{-# LANGUAGE NoImplicitPrelude #-}

-- | This module exports some /ready-to-use/ 'Animation's.

module Imj.Animation
    (
    -- * Explosive
      simpleExplosion
    , quantitativeExplosionThenSimpleExplosion
    -- * Free fall
    {- | 'freeFall' simulates the effect of gravity on an object that has an initial speed.

    'freeFallThenExplode' adds an explosion when the falling object hits the environment
     (ie when the 'InteractionResult' of an interaction between the object and
     the environment is 'Mutation').
    -}
    , freeFall
    , freeFallThenExplode
    -- * Fragments
    {- | 'fragmentsFreeFall' gives the impression that the object disintegrated in multiple
    pieces before falling.

    'fragmentsFreeFallThenExplode' adds an explosion when the falling object hits the environment
    (ie when the 'InteractionResult' of an interaction between the object and
    the environment is 'Mutation').
    -}
    , fragmentsFreeFall
    , fragmentsFreeFallThenExplode
    -- * Geometric
    , animatedPolygon
    , laserAnimation
    ) where

import           Imj.Prelude

import           Imj.Animation.Design
import           Imj.Animation.Geo
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Laser.Types

-- | A laser ray animation, with a fade-out effect.
laserAnimation :: LaserRay Actual
               -- ^ The laser ray
               -> KeyTime
               -- ^ 'KeyTime' of the game event that started this animation
               -> Animation
laserAnimation ray@(LaserRay _ (Ray seg)) keyTime =
  let collisionFree = fst $ extremities seg -- this needs to be collision-free
  in mkAnimation [laserAnimationGeo ray] keyTime SkipZero (Speed 1) collisionFree Nothing

-- | An animation chaining two circular explosions, the first explosion
-- can be configured in number of points, the second has 4*8=32 points.
quantitativeExplosionThenSimpleExplosion :: Int
                                         -- ^ Number of points in the first explosion
                                         -> Coords
                                         -- ^ Center of the first explosion
                                         -> KeyTime
                                         -- ^ 'KeyTime' of the game event that started this animation
                                         -> Speed
                                         -- ^ Animation speed
                                         -> Char
                                         -- ^ Character used when drawing the animation.
                                         -> Animation
quantitativeExplosionThenSimpleExplosion num pos keyTime animSpeed char =
  let funcs = [ quantitativeExplosionGeo num Interact
              , simpleExplosionGeo 8 Interact ]
  in mkAnimation funcs keyTime SkipZero animSpeed pos (Just char)

-- | An animation where a geometric figure (polygon or circle) expands then shrinks,
-- and doesn't interact with the environment.
animatedPolygon :: Int
                -- ^ If n==1, the geometric figure is a circle, else if n>1, a n-sided polygon
                -> Coords
                -- ^ Center of the polygon (or circle)
                -> KeyTime
                -- ^ 'KeyTime' of the game event that started this animation
                -> Speed
                -- ^ Animation speed
                -> Char
                -- ^ Character used when drawing the animation.
                -> Animation
animatedPolygon n pos keyTime animSpeed char =
  mkAnimation [animatePolygonGeo n] keyTime SkipZero animSpeed pos (Just char)

-- | A circular explosion configurable in number of points
simpleExplosion :: Int
                -- ^ Number of points in the explosion
                -> Coords
                -- ^ Center of the explosion
                -> KeyTime
                -- ^ 'KeyTime' of the game event that started this animation
                -> Speed
                -- ^ Animation speed
                -> Char
                -- ^ Character used when drawing the animation.
                -> Animation
simpleExplosion resolution pos keyTime animSpeed char =
  mkAnimation [simpleExplosionGeo resolution Interact] keyTime SkipZero animSpeed pos (Just char)

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts.
fragmentsFreeFall :: Vec2
                  -- ^ Initial speed
                  -> Coords
                  -- ^ Initial position
                  -> KeyTime
                  -- ^ 'KeyTime' of the game event that started this animation
                  -> Speed
                  -- ^ Animation speed
                  -> Char
                  -- ^ Character used when drawing the animation.
                  -> [Animation]
fragmentsFreeFall speed pos keyTime animSpeed char =
  map (\sp -> freeFall sp pos keyTime animSpeed char) $ variations speed

-- | A gravity-based free-falling animation.
freeFall :: Vec2
         -- ^ Initial speed
         -> Coords
         -- ^ Initial position
         -> KeyTime
         -- ^ 'KeyTime' of the game event that started this animation
         -> Speed
         -- ^ Animation speed
         -> Char
         -- ^ Character used when drawing the animation.
         -> Animation
freeFall speed pos keyTime animSpeed char =
  -- we want the WithZero here so that outer world animations start right next to
  -- a wall, even for the bottom wall.
  mkAnimation [gravityFallGeo speed Interact] keyTime WithZero animSpeed pos (Just char)

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts free-falling and then exploding.
fragmentsFreeFallThenExplode :: Vec2
                             -- ^ Initial speed
                             -> Coords
                             -- ^ Initial position
                             -> KeyTime
                             -- ^ 'KeyTime' of the game event that started this animation
                             -> Speed
                             -- ^ Animation speed
                             -> Char
                             -- ^ Character used when drawing the animation.
                             -> [Animation]
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
freeFallThenExplode :: Vec2
                    -- ^ Initial speed
                    -> Coords
                    -- ^ Initial position
                    -> KeyTime
                    -- ^ 'KeyTime' of the game event that started this animation
                    -> Speed
                    -- ^ Animation speed
                    -> Char
                    -- ^ Character used when drawing the animation.
                    -> Animation
freeFallThenExplode speed pos keyTime animSpeed char =
  let funcs = [ gravityFallGeo speed Interact
              , simpleExplosionGeo 8 Interact]
  -- WithZero looks better on collision between the ship and a number.
  in mkAnimation funcs keyTime WithZero animSpeed pos (Just char)
