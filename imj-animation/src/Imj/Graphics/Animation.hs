{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation
    (
    -- * Particle functions
    {- | In their pre-applied form ('VecPosSpeed' -> 'Frame' -> ['Particle']),
    /particle functions/ are used by an 'Animation' to produce 'Particle's.

    A /particle function/ may return either:

    * a constant number of 'Particle's across 'Frame's,
    * or 0 'Particle' to indicate that it will not produce any more 'Particle' in
    future 'Frame's.

    During an 'Animation' update, a 'Particle' produced by a /particle function/
    'CanInteract' with its environment, or be removed from the animation, if it is
    too far away (cf. 'EnvFunctions'). -}
      particles
    , particlesFreefall
    , particlesExplosion
    , particlesPolygonExpandShrink
    , particlesLaser
     -- * Animation
    , Animation
      {- | An 'Animation' generates 'Particle's: -}
    , Particle(..)
      -- | An 'Particle' can interact with its environment using 'EnvFunctions':
    , EnvFunctions(..)
    , CanInteract(..)
    , Distance(..)
    , InteractionResult(..)
      {- | 'Particle's live in a
      <https://en.wikipedia.org/wiki/Tree_(graph_theory) tree-like structure>: -}
    , Particles(..)
      -- ** Create
    , mkAnimation
    -- ** Update
    , getDeadline
    , shouldUpdate
    , updateAnimation
    -- ** Render
    , drawAnim
    -- * Predefined animations
    -- ** Explosive
    , simpleExplosion
    , quantitativeExplosionThenSimpleExplosion
    -- ** Free fall
    {- | 'freeFall' simulates the effect of gravity on an object that has an initial speed.

    'freeFallThenExplode' adds an explosion when the falling object hits the environment
     (ie when the 'InteractionResult' of an interaction between the object and
     the environment is 'Mutation').
    -}
    , freeFall
    , freeFallWithReboundsThenExplode
    , freeFallThenExplode
    -- ** Fragments
    {- | 'fragmentsFreeFall' gives the impression that the object disintegrated in multiple
    pieces before falling.

    'fragmentsFreeFallThenExplode' adds an explosion when the falling object is
    mutated by the environment.

    'fragmentsFreeFallWithReboundsThenExplode' adds rebounds before the final explosion.
    -}
    , fragmentsFreeFall
    , fragmentsFreeFallThenExplode
    , fragmentsFreeFallWithReboundsThenExplode
    -- ** Geometric
    , expandShrinkPolygon
    , laserAnimation
    -- ** Nice chars
    {-| 'niceChar' presents a list of 'Char's that /look good/
    when used in explosive and free fall animations. -}
    , niceChar
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
import           Imj.Graphics.Color
import           Imj.Iteration
import           Imj.Physics.Continuous.Types
import           Imj.Timing

defaultColors :: Frame -> LayeredColor
defaultColors = onBlack . colorFromFrame (rgb 4 0 0)

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
  | otherwise = mkAnimation posspeed [particlesLaser ray defaultColors] (Speed 1) envFunctions keyTime
 where
  -- speed doesn't matter to 'particlesLaser'
  posspeed = mkStaticVecPosSpeed $ pos2vec start
  envFunctions = EnvFunctions (const Stable) (const DistanceOK)

-- | An animation chaining two circular explosions, the first explosion
-- can be configured in number of particles, the second has 32 particles.
quantitativeExplosionThenSimpleExplosion :: Int
                                         -- ^ Number of particles in the first explosion
                                         -> Coords Pos
                                         -- ^ Center of the first explosion
                                         -> Char
                                         -- ^ Character used when drawing the animation.
                                         -> Speed
                                         -- ^ Animation speed
                                         -> EnvFunctions
                                         -> Either SystemTime KeyTime
                                         -- ^ 'Right' 'KeyTime' of the event's deadline
                                         -- that triggered this animation, or 'Left' 'SystemTime'
                                         -- of the current time if a player action triggered this animation
                                         -> Maybe Animation
quantitativeExplosionThenSimpleExplosion num pos char =
  let firstAngle = 2*pi / 5
      funcs = [ particles (explosion num firstAngle) zeroForceMotion Interact char defaultColors
              , particles (explosion 32 0) zeroForceMotion Interact char defaultColors]
       -- speed doesn't matter to 'particlesExplosionByCircle' and 'particlesExplosionByQuartArcs':
      posspeed = mkStaticVecPosSpeed $ pos2vec pos
  in mkAnimation posspeed funcs

-- | An animation where a geometric figure (polygon or circle) expands then shrinks,
-- and doesn't interact with the environment.
expandShrinkPolygon :: Int
                    -- ^ If n==1, the geometric figure is a circle, else if n>1, a n-sided polygon
                    -> Coords Pos
                    -- ^ Center of the polygon (or circle)
                    -> Speed
                    -> EnvFunctions
                    -- ^ Animation speed
                    -> Either SystemTime KeyTime
                    -- ^ 'Right' 'KeyTime' of the event's deadline
                    -- that triggered this animation, or 'Left' 'SystemTime'
                    -- of the current time if a player action triggered this animation
                    -> Maybe Animation
expandShrinkPolygon n pos =
  mkAnimation posspeed funcs
 where
  -- speed doesn't matter to 'particlesPolygonExpandShrink'
  posspeed = mkStaticVecPosSpeed $ pos2vec pos
  funcs = [particlesPolygonExpandShrink n defaultColors]

-- | A circular explosion configurable in number of particles
simpleExplosion :: Int
                -- ^ Number of particles in the explosion
                -> Coords Pos
                -- ^ Center of the explosion
                -> Char
                -- ^ Character used when drawing the animation.
                -> Speed
                -- ^ Animation speed
                -> EnvFunctions
                -> Either SystemTime KeyTime
                -- ^ 'Right' 'KeyTime' of the event's deadline
                -- that triggered this animation, or 'Left' 'SystemTime'
                -- of the current time if a player action triggered this animation
                -> Maybe Animation
simpleExplosion resolution pos char =
  mkAnimation posspeed funcs
 where
  -- speed doesn't matter to 'simpleExplosion'
  posspeed = mkStaticVecPosSpeed $ pos2vec pos
  funcs = [particles (explosion resolution 0) zeroForceMotion Interact char defaultColors]

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts.
fragmentsFreeFall :: Vec2 Vel
                  -- ^ Initial speed
                  -> Coords Pos
                  -- ^ Initial position
                  -> Char
                  -- ^ Character used when drawing the animation.
                  -> Speed
                  -- ^ Animation speed
                  -> EnvFunctions
                  -> Either SystemTime KeyTime
                  -- ^ 'Right' 'KeyTime' of the event's deadline
                  -- that triggered this animation, or 'Left' 'SystemTime'
                  -- of the current time if a player action triggered this animation
                  -> [Animation]
fragmentsFreeFall speed pos char animSpeed envFuncs keyTime =
  mapMaybe (\sp -> freeFall sp pos char animSpeed envFuncs keyTime) $ variations speed

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
                                         -> (Int -> Int -> Frame -> LayeredColor)
                                         -- ^ fragment index -> particle function level -> relative frame -> color
                                         -> Char
                                         -- ^ Character used when drawing the animation.
                                         -> Speed
                                         -- ^ Animation speed
                                         -> EnvFunctions
                                         -> Either SystemTime KeyTime
                                         -- ^ 'Right' 'KeyTime' of the event's deadline
                                         -- that triggered this animation, or 'Left' 'SystemTime'
                                         -- of the current time if a player action triggered this animation
                                         -> [Animation]
fragmentsFreeFallWithReboundsThenExplode speed pos velAtt nRebounds colorFuncs char animSpeed envFuncs keyTime =
  if velAtt <= 0
    then
      error "velocity attenuation should be > 0"
    else
      mapMaybe
        (\(idx,sp) ->
            freeFallWithReboundsThenExplode
              sp pos velAtt nRebounds (colorFuncs idx) char animSpeed envFuncs keyTime)
        $ zip [0..] $ variations speed

-- | A gravity-based free-falling animation.
freeFall :: Vec2 Vel
         -- ^ Initial speed
         -> Coords Pos
         -- ^ Initial position
         -> Char
         -- ^ Character used when drawing the animation.
         -> Speed
         -- ^ Animation speed
         -> EnvFunctions
         -> Either SystemTime KeyTime
         -- ^ 'Right' 'KeyTime' of the event's deadline
         -- that triggered this animation, or 'Left' 'SystemTime'
         -- of the current time if a player action triggered this animation
         -> Maybe Animation
freeFall speed pos char =
  mkAnimation posspeed funcs
 where
  posspeed = VecPosSpeed (pos2vec pos) speed
  funcs = [particlesFreefall 1 Interact char defaultColors]

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
                                -> (Int -> Frame -> LayeredColor)
                                -- ^ (particle function level -> relative frame -> color)
                                -> Char
                                -- ^ Character used when drawing the animation.
                                -> Speed
                                -- ^ Animation speed
                                -> EnvFunctions
                                -> Either SystemTime KeyTime
                                -- ^ 'Right' 'KeyTime' of the event's deadline
                                -- that triggered this animation, or 'Left' 'SystemTime'
                                -- of the current time if a player action triggered this animation
                                -> Maybe Animation
freeFallWithReboundsThenExplode speed pos velAtt nRebounds colorFuncs char =
  if velAtt <= 0
    then
      error "velocity attenuation should be > 0"
    else
      mkAnimation posspeed funcs
 where
  posspeed = VecPosSpeed (pos2vec pos) $ scalarProd (recip velAtt) speed
  nFragments = 16
  funcs = map (particlesFreefall velAtt Interact char . colorFuncs) [0..pred nRebounds]
          ++ [particles (explosion nFragments (pi/16)) gravityMotion Interact char (colorFuncs nRebounds)]

-- | Animation representing an object with an initial velocity disintegrating in
-- 4 different parts free-falling and then exploding.
fragmentsFreeFallThenExplode :: Vec2 Vel
                             -- ^ Initial speed
                             -> Coords Pos
                             -- ^ Initial position
                             -> Char
                             -- ^ Character used when drawing the animation.
                             -> Speed
                             -- ^ Animation speed
                             -> EnvFunctions
                             -> Either SystemTime KeyTime
                             -- ^ 'Right' 'KeyTime' of the event's deadline
                             -- that triggered this animation, or 'Left' 'SystemTime'
                             -- of the current time if a player action triggered this animation
                             -> [Animation]
fragmentsFreeFallThenExplode speed pos c s envFuncs k =
  mapMaybe (\sp -> freeFallThenExplode sp pos c s envFuncs k) $ variations speed

-- | Given an input speed, computes four slightly different input speeds
variations :: Vec2 Vel -> [Vec2 Vel]
variations sp =
  map (sumVec2d sp) [ Vec2 0.12     (-0.16)
                    , Vec2 (-0.22) (-0.116)
                    , Vec2 (-0.04)  0.36
                    , Vec2 0.48     0.08]

-- | An animation chaining a gravity-based free-fall and a circular explosion of 32 particles.
freeFallThenExplode :: Vec2 Vel
                    -- ^ Initial speed
                    -> Coords Pos
                    -- ^ Initial position
                    -> Char
                    -- ^ Character used when drawing the animation.
                    -> Speed
                    -- ^ Animation speed
                    -> EnvFunctions
                    -> Either SystemTime KeyTime
                    -- ^ 'Right' 'KeyTime' of the event's deadline
                    -- that triggered this animation, or 'Left' 'SystemTime'
                    -- of the current time if a player action triggered this animation
                    -> Maybe Animation
freeFallThenExplode speed pos char =
  let funcs = [ particlesFreefall 1.0 Interact char defaultColors
              , particles (explosion 32 0) zeroForceMotion Interact char defaultColors]
  in mkAnimation (VecPosSpeed (pos2vec pos) speed) funcs
