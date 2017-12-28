{-# LANGUAGE NoImplicitPrelude #-}

-- | Some /ready-to-use/ 'Animation's.

module Imj.Graphics.Animation
    (
    -- * Animations
    -- ** Explosive
      simpleExplosion
    , quantitativeExplosionThenSimpleExplosion
    -- ** Free fall
    {- | 'freeFall' simulates the effect of gravity on an object that has an initial speed.

    'freeFallThenExplode' adds an explosion when the falling object hits the environment
     (ie when the 'InteractionResult' of an interaction between the object and
     the environment is 'Mutation').
    -}
    , freeFall
    , freeFallThenExplode
    -- ** Fragments
    {- | 'fragmentsFreeFall' gives the impression that the object disintegrated in multiple
    pieces before falling.

    'fragmentsFreeFallThenExplode' adds an explosion when the falling object hits the environment
    (ie when the 'InteractionResult' of an interaction between the object and
    the environment is 'Mutation').
    -}
    , fragmentsFreeFall
    , fragmentsFreeFallThenExplode
    -- ** Geometric
    , animatedPolygon
    , laserAnimation
    -- * Chars
    {-| 'niceChar' cycles through a list of 'Char's that /look good/ for
    explosive animations. In Hamazed, they are used to create some /outer world/
    animations. -}
    , niceChar
    , module Imj.Graphics.Animation.Geo
    ) where

import           Imj.Prelude

import           Imj.GameItem.Weapon.Laser.Types
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.Animation.Chars
import           Imj.Graphics.Animation.Design
import           Imj.Graphics.Animation.Geo

-- | A laser ray animation, with a fade-out effect.
laserAnimation :: LaserRay Actual
               -- ^ The laser ray
               -> KeyTime
               -- ^ 'KeyTime' of the game event that started this animation
               -> Animation
laserAnimation ray@(LaserRay _ (Ray seg)) keyTime =
  let collisionFree = fst $ extremities seg -- this needs to be collision-free
  in mkAnimation [laserAnimationGeo ray] keyTime (Speed 1) collisionFree Nothing

-- | An animation chaining two circular explosions, the first explosion
-- can be configured in number of points, the second has 4*8=32 points.
quantitativeExplosionThenSimpleExplosion :: Int
                                         -- ^ Number of points in the first explosion
                                         -> Coords Pos
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
  in mkAnimation funcs keyTime animSpeed pos (Just char)

-- | An animation where a geometric figure (polygon or circle) expands then shrinks,
-- and doesn't interact with the environment.
animatedPolygon :: Int
                -- ^ If n==1, the geometric figure is a circle, else if n>1, a n-sided polygon
                -> Coords Pos
                -- ^ Center of the polygon (or circle)
                -> KeyTime
                -- ^ 'KeyTime' of the game event that started this animation
                -> Speed
                -- ^ Animation speed
                -> Char
                -- ^ Character used when drawing the animation.
                -> Animation
animatedPolygon n pos keyTime animSpeed char =
  mkAnimation [animatePolygonGeo n] keyTime animSpeed pos (Just char)

-- | A circular explosion configurable in number of points
simpleExplosion :: Int
                -- ^ Number of points in the explosion
                -> Coords Pos
                -- ^ Center of the explosion
                -> KeyTime
                -- ^ 'KeyTime' of the game event that started this animation
                -> Speed
                -- ^ Animation speed
                -> Char
                -- ^ Character used when drawing the animation.
                -> Animation
simpleExplosion resolution pos keyTime animSpeed char =
  mkAnimation [simpleExplosionGeo resolution Interact] keyTime animSpeed pos (Just char)

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts.
fragmentsFreeFall :: Vec2 Vel
                  -- ^ Initial speed
                  -> Coords Pos
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
freeFall :: Vec2 Vel
         -- ^ Initial speed
         -> Coords Pos
         -- ^ Initial position
         -> KeyTime
         -- ^ 'KeyTime' of the game event that started this animation
         -> Speed
         -- ^ Animation speed
         -> Char
         -- ^ Character used when drawing the animation.
         -> Animation
freeFall speed pos keyTime animSpeed char =
  mkAnimation [gravityFallGeo speed Interact] keyTime animSpeed pos (Just char)

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts free-falling and then exploding.
fragmentsFreeFallThenExplode :: Vec2 Vel
                             -- ^ Initial speed
                             -> Coords Pos
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
variations :: Vec2 Vel -> [Vec2 Vel]
variations sp =
  map (sumVec2d sp) [ Vec2 0.12     (-0.16)
                    , Vec2 (-0.22) (-0.116)
                    , Vec2 (-0.04)  0.36
                    , Vec2 0.48     0.08]

-- | An animation chaining a gravity-based free-fall and a circular explosion of 4*8 points.
freeFallThenExplode :: Vec2 Vel
                    -- ^ Initial speed
                    -> Coords Pos
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
  in mkAnimation funcs keyTime animSpeed pos (Just char)
