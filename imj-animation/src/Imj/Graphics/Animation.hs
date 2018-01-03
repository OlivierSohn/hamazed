{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation
    (
    -- * Animation functions
    -- | Animation functions are used by 'Animation's to update 'AnimatedPoint's.
      gravityFallGeo
    , simpleExplosionGeo
    , quantitativeExplosionGeo
    , animatePolygonGeo
    , laserAnimationGeo
     -- * Animation
    , Animation
      {- | An 'Animation' generates 'AnimatedPoint's: -}
    , AnimatedPoint(..)
    -- | 'AnimatedPoint's can interact with their environment:
    , CanInteract(..)
    -- | The result of an interaction between an 'AnimatedPoint' and its environment
    -- can trigger a /mutation/:
    , InteractionResult(..)
      {- | 'AnimatedPoint's live in a
      <https://en.wikipedia.org/wiki/Tree_(graph_theory) tree-like structure>:
      -}
    , AnimatedPoints(..)
      -- ** Create
    , mkAnimation
    -- ** Predefined animations
    -- *** Explosive
    , simpleExplosion
    , quantitativeExplosionThenSimpleExplosion
    -- *** Free fall
    {- | 'freeFall' simulates the effect of gravity on an object that has an initial speed.

    'freeFallThenExplode' adds an explosion when the falling object hits the environment
     (ie when the 'InteractionResult' of an interaction between the object and
     the environment is 'Mutation').
    -}
    , freeFall
    , freeFallThenExplode
    -- *** Fragments
    {- | 'fragmentsFreeFall' gives the impression that the object disintegrated in multiple
    pieces before falling.

    'fragmentsFreeFallThenExplode' adds an explosion when the falling object hits the environment
    (ie when the 'InteractionResult' of an interaction between the object and
    the environment is 'Mutation').
    -}
    , fragmentsFreeFall
    , fragmentsFreeFallThenExplode
    -- *** Geometric
    , animatedPolygon
    , laserAnimation
    -- *** Nice chars
    {-| 'niceChar' presents a list of 'Char's that /look good/
    when used in explosive and free fall animations. -}
    , niceChar
    -- ** Update
    , getDeadline
    , shouldUpdate
    , updateAnimation
    -- ** Render
    , drawAnim
    -- * Internal
    , module Imj.Graphics.Animation.Internal
    -- * Reexports
    , module Imj.Timing
    , module Imj.Iteration
    , Coords
    ) where

import           Imj.Prelude

import           Imj.GameItem.Weapon.Laser.Types
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.Animation.Chars
import           Imj.Graphics.Animation.Design.Create
import           Imj.Graphics.Animation.Design.Color
import           Imj.Graphics.Animation.Design.Draw
import           Imj.Graphics.Animation.Design.Timing
import           Imj.Graphics.Animation.Design.Types
import           Imj.Graphics.Animation.Design.Update
import           Imj.Graphics.Animation.Geo
import           Imj.Graphics.Animation.Internal
import           Imj.Iteration
import           Imj.Timing

-- | A laser ray animation, with a fade-out effect.
laserAnimation :: LaserRay Actual
               -- ^ The laser ray
               -> (Coords Pos -> InteractionResult)
               -- ^ Environment interaction function
               -> Either SystemTime KeyTime
               -- ^ 'Right' 'KeyTime' of the event's deadline
               -- that triggered this animation, or 'Left' 'SystemTime'
               -- of the current time if a player action triggered this animation
               -> Maybe Animation
laserAnimation ray@(LaserRay _ start len) interaction keyTime
  | len == 0  = Nothing
  | otherwise = mkAnimation start [laserAnimationGeo ray] (Speed 1) interaction keyTime Nothing

-- | An animation chaining two circular explosions, the first explosion
-- can be configured in number of points, the second has 4*8=32 points.
quantitativeExplosionThenSimpleExplosion :: Int
                                         -- ^ Number of points in the first explosion
                                         -> Coords Pos
                                         -- ^ Center of the first explosion
                                         -> (Coords Pos -> InteractionResult)
                                         -- ^ Environment interaction function
                                         -> Speed
                                         -- ^ Animation speed
                                         -> Either SystemTime KeyTime
                                         -- ^ 'Right' 'KeyTime' of the event's deadline
                                         -- that triggered this animation, or 'Left' 'SystemTime'
                                         -- of the current time if a player action triggered this animation
                                         -> Char
                                         -- ^ Character used when drawing the animation.
                                         -> Maybe Animation
quantitativeExplosionThenSimpleExplosion num pos interaction animSpeed keyTime char =
  let funcs = [ quantitativeExplosionGeo num Interact
              , simpleExplosionGeo 8 Interact ]
  in mkAnimation pos funcs animSpeed interaction keyTime (Just char)

-- | An animation where a geometric figure (polygon or circle) expands then shrinks,
-- and doesn't interact with the environment.
animatedPolygon :: Int
                -- ^ If n==1, the geometric figure is a circle, else if n>1, a n-sided polygon
                -> Coords Pos
                -- ^ Center of the polygon (or circle)
                -> (Coords Pos -> InteractionResult)
                -- ^ Environment interaction function
                -> Speed
                -- ^ Animation speed
                -> Either SystemTime KeyTime
                -- ^ 'Right' 'KeyTime' of the event's deadline
                -- that triggered this animation, or 'Left' 'SystemTime'
                -- of the current time if a player action triggered this animation
                -> Char
                -- ^ Character used when drawing the animation.
                -> Maybe Animation
animatedPolygon n pos interaction animSpeed keyTime char =
  mkAnimation pos [animatePolygonGeo n] animSpeed interaction keyTime (Just char)

-- | A circular explosion configurable in number of points
simpleExplosion :: Int
                -- ^ Number of points in the explosion
                -> Coords Pos
                -- ^ Center of the explosion
                -> (Coords Pos -> InteractionResult)
                -- ^ Environment interaction function
                -> Speed
                -- ^ Animation speed
                -> Either SystemTime KeyTime
                -- ^ 'Right' 'KeyTime' of the event's deadline
                -- that triggered this animation, or 'Left' 'SystemTime'
                -- of the current time if a player action triggered this animation
                -> Char
                -- ^ Character used when drawing the animation.
                -> Maybe Animation
simpleExplosion resolution pos interaction animSpeed keyTime char =
  mkAnimation pos [simpleExplosionGeo resolution Interact] animSpeed interaction keyTime (Just char)

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts.
fragmentsFreeFall :: Vec2 Vel
                  -- ^ Initial speed
                  -> Coords Pos
                  -- ^ Initial position
                  -> (Coords Pos -> InteractionResult)
                  -- ^ Environment interaction function
                  -> Speed
                  -- ^ Animation speed
                  -> Either SystemTime KeyTime
                  -- ^ 'Right' 'KeyTime' of the event's deadline
                  -- that triggered this animation, or 'Left' 'SystemTime'
                  -- of the current time if a player action triggered this animation
                  -> Char
                  -- ^ Character used when drawing the animation.
                  -> [Animation]
fragmentsFreeFall speed pos interaction animSpeed keyTime char =
  mapMaybe (\sp -> freeFall sp pos interaction animSpeed keyTime char) $ variations speed

-- | A gravity-based free-falling animation.
freeFall :: Vec2 Vel
         -- ^ Initial speed
         -> Coords Pos
         -- ^ Initial position
         -> (Coords Pos -> InteractionResult)
         -- ^ Environment interaction function
         -> Speed
         -- ^ Animation speed
         -> Either SystemTime KeyTime
         -- ^ 'Right' 'KeyTime' of the event's deadline
         -- that triggered this animation, or 'Left' 'SystemTime'
         -- of the current time if a player action triggered this animation
         -> Char
         -- ^ Character used when drawing the animation.
         -> Maybe Animation
freeFall speed pos interaction animSpeed keyTime char =
  mkAnimation pos [gravityFallGeo speed Interact] animSpeed interaction keyTime (Just char)

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts free-falling and then exploding.
fragmentsFreeFallThenExplode :: Vec2 Vel
                             -- ^ Initial speed
                             -> Coords Pos
                             -- ^ Initial position
                             -> (Coords Pos -> InteractionResult)
                             -- ^ Environment interaction function
                             -> Speed
                             -- ^ Animation speed
                             -> Either SystemTime KeyTime
                             -- ^ 'Right' 'KeyTime' of the event's deadline
                             -- that triggered this animation, or 'Left' 'SystemTime'
                             -- of the current time if a player action triggered this animation
                             -> Char
                             -- ^ Character used when drawing the animation.
                             -> [Animation]
fragmentsFreeFallThenExplode speed pos interaction k s c =
  mapMaybe (\sp -> freeFallThenExplode sp pos interaction k s c) $ variations speed

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
                    -> (Coords Pos -> InteractionResult)
                    -- ^ Environment interaction function
                    -> Speed
                    -- ^ Animation speed
                    -> Either SystemTime KeyTime
                    -- ^ 'Right' 'KeyTime' of the event's deadline
                    -- that triggered this animation, or 'Left' 'SystemTime'
                    -- of the current time if a player action triggered this animation
                    -> Char
                    -- ^ Character used when drawing the animation.
                    -> Maybe Animation
freeFallThenExplode speed pos interaction animSpeed keyTime char =
  let funcs = [ gravityFallGeo speed Interact
              , simpleExplosionGeo 8 Interact]
  in mkAnimation pos funcs animSpeed interaction keyTime (Just char)
