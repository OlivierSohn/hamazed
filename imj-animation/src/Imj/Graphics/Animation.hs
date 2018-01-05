{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation
    (
    -- * Animation functions
    {- | In their pre-applied form ('VecPosSpeed' -> 'Frame' -> ['AnimatedPoint']),
    /animation functions/ are used by an 'Animation' to produce 'AnimatedPoint's.

    They are expected to return a /constant/ number of 'AnimatedPoint's across frames,
    except when they know they reached the end of their animation, then they return 0
    'AnimatedPoint's.

    Some /animated functions/ will produce 'AnimatedPoint's /forever/. This doesn't
    imply that the 'Animation' that uses them will go on forever, because
    'AnimatedPoint's can be mutated by the environment, or removed when they go
    too far away (cf. 'EnvFunctions').

    However, /animation functions/ producing 'AnimatedPoint's that 'DontInteract'
    are required to eventually finish because the 'AnimatedPoints' they generate
    cannot be mutated by the environment or be removed from the animation when they
    are too far (as explained in 'EnvFunctions' and 'CanInteract'). -}
      gravityFallGeo
    , simpleExplosionGeo
    , quantitativeExplosionGeo
    , animatePolygonGeo
    , laserAnimationGeo
     -- * Animation
    , Animation
      {- | An 'Animation' generates 'AnimatedPoint's: -}
    , AnimatedPoint(..)
      -- | An 'AnimatedPoint' can interact with its environment using 'EnvFunctions':
    , EnvFunctions(..)
    , CanInteract(..)
    , Distance(..)
    , InteractionResult(..)
      {- | 'AnimatedPoint's live in a
      <https://en.wikipedia.org/wiki/Tree_(graph_theory) tree-like structure>: -}
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
    , freeFallWithReboundsThenExplode
    , freeFallThenExplode
    -- *** Fragments
    {- | 'fragmentsFreeFall' gives the impression that the object disintegrated in multiple
    pieces before falling.

    'fragmentsFreeFallThenExplode' adds an explosion when the falling object hits the environment
    (ie when the 'InteractionResult' of an interaction between the object and
    the environment is 'Mutation').
    -}
    , fragmentsFreeFall
    , fragmentsFreeFallWithReboundsThenExplode
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
import           Imj.Physics.Continuous.Types
import           Imj.Timing

-- | A laser ray animation, with a fade-out effect.
laserAnimation :: LaserRay Actual
               -- ^ The laser ray
               -> Either SystemTime KeyTime
               -- ^ 'Right' 'KeyTime' of the event's deadline
               -- that triggered this animation, or 'Left' 'SystemTime'
               -- of the current time if a player action triggered this animation
               -> Maybe Animation
laserAnimation ray@(LaserRay _ start len) keyTime
  | len == 0  = Nothing
  | otherwise = mkAnimation posspeed [laserAnimationGeo ray] (Speed 1) envFunctions keyTime Nothing
 where
  -- speed doesn't matter to 'laserAnimationGeo'
  posspeed = mkStaticVecPosSpeed $ pos2vec start
  envFunctions = EnvFunctions (const Stable) (const DistanceOK)

-- | An animation chaining two circular explosions, the first explosion
-- can be configured in number of points, the second has 4*8=32 points.
quantitativeExplosionThenSimpleExplosion :: Int
                                         -- ^ Number of points in the first explosion
                                         -> Coords Pos
                                         -- ^ Center of the first explosion
                                         -> EnvFunctions
                                         -> Speed
                                         -- ^ Animation speed
                                         -> Either SystemTime KeyTime
                                         -- ^ 'Right' 'KeyTime' of the event's deadline
                                         -- that triggered this animation, or 'Left' 'SystemTime'
                                         -- of the current time if a player action triggered this animation
                                         -> Char
                                         -- ^ Character used when drawing the animation.
                                         -> Maybe Animation
quantitativeExplosionThenSimpleExplosion num pos envFuncs animSpeed keyTime char =
  let funcs = [ quantitativeExplosionGeo num Interact
              , simpleExplosionGeo 8 Interact ]
       -- speed doesn't matter to 'quantitativeExplosionGeo' and 'simpleExplosionGeo'
      posspeed = mkStaticVecPosSpeed $ pos2vec pos
  in mkAnimation posspeed funcs animSpeed envFuncs keyTime (Just char)

-- | An animation where a geometric figure (polygon or circle) expands then shrinks,
-- and doesn't interact with the environment.
animatedPolygon :: Int
                -- ^ If n==1, the geometric figure is a circle, else if n>1, a n-sided polygon
                -> Coords Pos
                -- ^ Center of the polygon (or circle)
                -> EnvFunctions
                -> Speed
                -- ^ Animation speed
                -> Either SystemTime KeyTime
                -- ^ 'Right' 'KeyTime' of the event's deadline
                -- that triggered this animation, or 'Left' 'SystemTime'
                -- of the current time if a player action triggered this animation
                -> Char
                -- ^ Character used when drawing the animation.
                -> Maybe Animation
animatedPolygon n pos envFuncs animSpeed keyTime char =
  mkAnimation posspeed [animatePolygonGeo n] animSpeed envFuncs keyTime (Just char)
 where
  -- speed doesn't matter to 'animatePolygonGeo'
  posspeed = mkStaticVecPosSpeed $ pos2vec pos

-- | A circular explosion configurable in number of points
simpleExplosion :: Int
                -- ^ Number of points in the explosion
                -> Coords Pos
                -- ^ Center of the explosion
                -> EnvFunctions
                -> Speed
                -- ^ Animation speed
                -> Either SystemTime KeyTime
                -- ^ 'Right' 'KeyTime' of the event's deadline
                -- that triggered this animation, or 'Left' 'SystemTime'
                -- of the current time if a player action triggered this animation
                -> Char
                -- ^ Character used when drawing the animation.
                -> Maybe Animation
simpleExplosion resolution pos envFuncs animSpeed keyTime char =
  mkAnimation posspeed [simpleExplosionGeo resolution Interact] animSpeed envFuncs keyTime (Just char)
 where
  -- speed doesn't matter to 'simpleExplosion'
  posspeed = mkStaticVecPosSpeed $ pos2vec pos

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts.
fragmentsFreeFall :: Vec2 Vel
                  -- ^ Initial speed
                  -> Coords Pos
                  -- ^ Initial position
                  -> EnvFunctions
                  -> Speed
                  -- ^ Animation speed
                  -> Either SystemTime KeyTime
                  -- ^ 'Right' 'KeyTime' of the event's deadline
                  -- that triggered this animation, or 'Left' 'SystemTime'
                  -- of the current time if a player action triggered this animation
                  -> Char
                  -- ^ Character used when drawing the animation.
                  -> [Animation]
fragmentsFreeFall speed pos envFuncs animSpeed keyTime char =
  mapMaybe (\sp -> freeFall sp pos envFuncs animSpeed keyTime char) $ variations speed

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts and rebounding several times until it explodes.
fragmentsFreeFallWithReboundsThenExplode :: Vec2 Vel
                                         -- ^ Initial speed
                                         -> Coords Pos
                                         -- ^ Initial position
                                         -> Float
                                         -- ^ Rebound speed attenuation factor, expected to be strictly positive.
                                         -> Int
                                         -- ^ Number of rebounds
                                         -> EnvFunctions
                                         -> Speed
                                         -- ^ Animation speed
                                         -> Either SystemTime KeyTime
                                         -- ^ 'Right' 'KeyTime' of the event's deadline
                                         -- that triggered this animation, or 'Left' 'SystemTime'
                                         -- of the current time if a player action triggered this animation
                                         -> Char
                                         -- ^ Character used when drawing the animation.
                                         -> [Animation]
fragmentsFreeFallWithReboundsThenExplode speed pos velAtt nRebounds envFuncs animSpeed keyTime char =
  if velAtt <= 0
    then
      error "velocity attenuation should be > 0"
    else
      mapMaybe
        (\sp -> freeFallWithReboundsThenExplode sp pos velAtt nRebounds envFuncs animSpeed keyTime char)
        $ variations speed

-- | A gravity-based free-falling animation.
freeFall :: Vec2 Vel
         -- ^ Initial speed
         -> Coords Pos
         -- ^ Initial position
         -> EnvFunctions
         -> Speed
         -- ^ Animation speed
         -> Either SystemTime KeyTime
         -- ^ 'Right' 'KeyTime' of the event's deadline
         -- that triggered this animation, or 'Left' 'SystemTime'
         -- of the current time if a player action triggered this animation
         -> Char
         -- ^ Character used when drawing the animation.
         -> Maybe Animation
freeFall speed pos envFuncs animSpeed keyTime char =
  mkAnimation posspeed funcs animSpeed envFuncs keyTime (Just char)
 where
  posspeed = VecPosSpeed (pos2vec pos) speed
  funcs = [gravityFallGeo 1.0 Interact]

-- | A gravity-based free-falling animation, with several rebounds and a final
-- explosion.
freeFallWithReboundsThenExplode :: Vec2 Vel
                                -- ^ Initial speed
                                -> Coords Pos
                                -- ^ Initial position
                                -> Float
                                -- ^ Velocity attenuation factor on rebound, expected to be strictly positive.
                                -> Int
                                -- ^ Number of rebounds
                                -> EnvFunctions
                                -> Speed
                                -- ^ Animation speed
                                -> Either SystemTime KeyTime
                                -- ^ 'Right' 'KeyTime' of the event's deadline
                                -- that triggered this animation, or 'Left' 'SystemTime'
                                -- of the current time if a player action triggered this animation
                                -> Char
                                -- ^ Character used when drawing the animation.
                                -> Maybe Animation
freeFallWithReboundsThenExplode speed pos velAtt nRebounds envFuncs animSpeed keyTime char =
  if velAtt <= 0
    then
      error "velocity attenuation should be > 0"
    else
      mkAnimation posspeed funcs animSpeed envFuncs keyTime (Just char)
 where
  posspeed = VecPosSpeed (pos2vec pos) $ scalarProd (recip velAtt) speed
  funcs = replicate nRebounds (gravityFallGeo velAtt Interact) ++ [simpleExplosionGeo nRebounds Interact]

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts free-falling and then exploding.
fragmentsFreeFallThenExplode :: Vec2 Vel
                             -- ^ Initial speed
                             -> Coords Pos
                             -- ^ Initial position
                             -> EnvFunctions
                             -> Speed
                             -- ^ Animation speed
                             -> Either SystemTime KeyTime
                             -- ^ 'Right' 'KeyTime' of the event's deadline
                             -- that triggered this animation, or 'Left' 'SystemTime'
                             -- of the current time if a player action triggered this animation
                             -> Char
                             -- ^ Character used when drawing the animation.
                             -> [Animation]
fragmentsFreeFallThenExplode speed pos envFuncs k s c =
  mapMaybe (\sp -> freeFallThenExplode sp pos envFuncs k s c) $ variations speed

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
                    -> EnvFunctions
                    -> Speed
                    -- ^ Animation speed
                    -> Either SystemTime KeyTime
                    -- ^ 'Right' 'KeyTime' of the event's deadline
                    -- that triggered this animation, or 'Left' 'SystemTime'
                    -- of the current time if a player action triggered this animation
                    -> Char
                    -- ^ Character used when drawing the animation.
                    -> Maybe Animation
freeFallThenExplode speed pos envFuncs animSpeed keyTime char =
  let funcs = [ gravityFallGeo 1.0 Interact
              , simpleExplosionGeo 8 Interact]
  in mkAnimation (VecPosSpeed (pos2vec pos) speed) funcs animSpeed envFuncs keyTime (Just char)
