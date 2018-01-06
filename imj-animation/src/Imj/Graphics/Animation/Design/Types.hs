{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation.Design.Types
          ( Animation(..)
          , EnvFunctions(..)
          , Distance(..)
          , UpdateSpec(..)
          , Particles(..)
          , Particle(..)
          , CanInteract(..)
          , InteractionResult(..)
          -- Reexports
          , VecPosSpeed
          , LayeredColor
          ) where

import           Imj.Prelude

import           GHC.Show(showString)

import           Imj.Iteration
import           Imj.Graphics.Color.Types
import           Imj.Geo.Discrete.Types
import           Imj.Physics.Continuous.Types
import           Imj.Timing


data Animation = Animation {
    _animationPoints :: !Particles
    -- ^ The current points.
  , _animationUpdate :: !(EnvFunctions
                      -> Frame
                      -> Particles
                      -> Particles)
    -- ^ The function updating 'Particles'.
  , _animationEnvFunctions :: !EnvFunctions
    -- ^ The environment functiosn.
  , _animationNextUpdateSpec :: !UpdateSpec
    -- ^ The time and iteration of the next update
}

-- | Functions to interact with the environment
data EnvFunctions = EnvFunctions {
    _envFunctionsInteract :: !(Coords Pos -> InteractionResult)
    {- ^ /Interaction/ function.

    During update, 'Particle's for which this returns 'Mutation' can mutate
    if they are allowed to.

    During draw, 'Particle's for which this
    function returns 'Stable' are drawn. Others are
    not drawn because they would overlap with the environment. -}
  , _envFunctionsDistance :: !(Vec2 Pos -> Distance)
    {- ^ /Distance/ function.

    During update, 'Particle's which 'CanInteract' with the environment, and
    for which this function returns 'TooFar' are removed from the animation
    (i.e, they are converted to a 'Particles' at their location,
    and this 'Particles' will not evolve because its center, too will be 'TooFar'.).

    Note that 'Particle's that 'DontInteract' with the environment will not
    be affected. As of today, particle functions generating 'Particle's that
    'DontInteract' don't diverge, so it's not an issue. In case new types of animation
    functions are developped, we could change this behaviour. -}
}

data Distance = DistanceOK
              | TooFar
              -- ^ When an 'Particle' is too far, the animation removes it.
              deriving(Show, Eq)


instance Show Animation where
  showsPrec _ (Animation a _ _ b) =
    showString $ "Animation{" ++ show (a,b) ++ "}"


data UpdateSpec = UpdateSpec {
    _updateSpecTime :: !KeyTime
    -- ^ The time at which the update should happen.
  , _updateSpecIteration :: !Iteration
    -- ^ The iteration that will be used in the update.
} deriving(Show)


data Particles = Particles {
    _particlesBranches :: !(Maybe [Either Particles Particle])
    -- ^ The children. A child can be 'Right' 'Particle' or 'Left' 'Particles'.
    --
    --  When a 'Right' 'Particle' mutates, it is converted to an empty 'Left' 'Particles'
  , _particlesCenter :: !VecPosSpeed
    -- ^ The center, aka the position and speed of the 'Particle', w.r.t the animation reference frame,
    -- that gave birth to this 'Particles'.
  , _particlesFrame :: !Frame
    -- ^ The frame at which this 'Particles' was created, relatively to the parent, if any.
} deriving (Show)

data Particle = Particle {
    _particleCanInteract :: !CanInteract
    -- ^ Can the particle interact with the environment?
  , _particleVecPosSpeed :: {-# UNPACK #-} !VecPosSpeed
    -- ^ Continuous location and speed, w.r.t the animation reference frame.
  , _particleDrawnWith :: !Char
    -- ^ The char used to draw the particle.
  , _particleColor :: !LayeredColor
  -- ^ The color used to draw the particle.
} deriving (Show)

data CanInteract = DontInteract
                 {- ^ The 'Particle' can't interact with the environment.

                 To ensure that the 'Animation' is finite in time,
                 particle functions returning 'Particle's that
                 'DontInteract' should return an empty list of 'Particle's
                 for each 'Frame' after a given 'Frame'. -}
                 | Interact
                 {- ^ The 'Particle' can be mutated after an interaction
                 with the environment.

                 For the animation to be finite in time, 'Particle's that
                 'Interact' /must/ eventually be either mutated by the environment
                 or be declared 'TooFar'.

                 Hence, particle functions returning 'Particle's that
                 'Interact' can guarantee animation finitude by computing their
                 coordinates using functions /diverging/ in the 'Frame' argument:

                 * if the environment has a finite size, they will eventually mutate
                 * if the environment is infinite, we rely on the distance function in 'EnvFunctions'
                 to eventually return 'TooFar'. -}
                 deriving (Show, Eq)

data InteractionResult = Mutation
                       -- ^ The 'Particle' can mutate.
                       | Stable
                       -- ^ The 'Particle' doesn't mutate.
                       deriving(Eq,Show)
