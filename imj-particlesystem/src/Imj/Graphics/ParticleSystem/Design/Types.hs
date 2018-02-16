{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Design.Types
          ( ParticleSystem(..)
          , EnvFunctions(..)
          , Distance(..)
          , UpdateSpec(..)
          , ParticleTree(..)
          , Particle(..)
          , CanInteract(..)
          , InteractionResult(..)
          -- Reexports
          , ParticleSyst
          , VecPosSpeed
          , LayeredColor
          ) where

import           Imj.Prelude

import           Imj.Iteration
import           Imj.Graphics.Color.Types
import           Imj.Graphics.ParticleSystem.Design.Timing
import           Imj.Geo.Discrete.Types
import           Imj.Physics.Continuous.Types
import           Imj.Timing


data ParticleSystem = ParticleSystem {
    _particleSystemPoints :: !ParticleTree
  , _particleSystemUpdate :: !(EnvFunctions
                            -> Frame
                            -> ParticleTree
                            -> ParticleTree)
  , _particleSystemEnvFunctions :: {-# UNPACK #-} !EnvFunctions
  , _particleSystemNextUpdateSpec :: !UpdateSpec
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
    for which this function returns 'TooFar' are removed from the particle system
    (i.e, they are converted to a 'ParticleTree' at their location,
    and this 'ParticleTree' will not evolve because its center, too will be 'TooFar'.).

    Note that 'Particle's that 'DontInteract' with the environment will not
    be affected. As of today, particle functions generating 'Particle's that
    'DontInteract' don't diverge, so it's not an issue. In case new types of particle
    functions are developped, we could change this behaviour. -}
}

data Distance = DistanceOK
              | TooFar
              -- ^ When a 'Particle' is too far, it is not rendered and not updated anymore.
              deriving(Show, Eq)


instance Show ParticleSystem where
  show (ParticleSystem a _ _ b) = "ParticleSystem{" ++ show (a,b) ++ "}"


data UpdateSpec = UpdateSpec {
    _updateSpecTime :: {-# UNPACK #-} !(Time Point ParticleSyst)
    -- ^ The time at which the update should happen.
  , _updateSpecIteration :: {-# UNPACK #-} !Iteration
    -- ^ The iteration that will be used in the update.
} deriving(Show)


data ParticleTree = ParticleTree {
    _particleTreeBranches :: !(Maybe [Either ParticleTree Particle])
    -- ^ The children. A child can be 'Right' 'Particle' or 'Left' 'ParticleTree'.
    --
    --  When a 'Right' 'Particle' mutates, it is converted to an empty 'Left' 'ParticleTree'
  , _particleTreeCenter :: {-# UNPACK #-} !VecPosSpeed
    -- ^ The center, aka the position and speed of the 'Particle', w.r.t the particle system reference frame,
    -- that gave birth to this 'ParticleTree'.
  , _particleTreeFrame :: {-# UNPACK #-} !Frame
    -- ^ The frame at which this 'ParticleTree' was created, relatively to the parent, if any.
} deriving (Show)

data Particle = Particle {
    _particleCanInteract :: {-unpack sum-} !CanInteract
    -- ^ Can the particle interact with the environment?
  , _particleVecPosSpeed :: {-# UNPACK #-} !VecPosSpeed
    -- ^ Continuous location and speed, w.r.t the particle system reference frame.
  , _particleDrawnWith :: {-# UNPACK #-} !Char
    -- ^ The char used to draw the particle.
  , _particleColor :: {-# UNPACK #-} !LayeredColor
  -- ^ The color used to draw the particle.
} deriving (Show)

data CanInteract = DontInteract
                 {- ^ The 'Particle' can't interact with the environment.

                 To ensure that the 'ParticleSystem' is finite in time,
                 particle functions returning 'Particle's that
                 'DontInteract' should return an empty list of 'Particle's
                 for each 'Frame' after a given 'Frame'. -}
                 | Interact
                 {- ^ The 'Particle' can be mutated after an interaction
                 with the environment.

                 For the particle system to be finite in time, 'Particle's that
                 'Interact' /must/ eventually be either mutated by the environment
                 or be declared 'TooFar'.

                 Hence, particle functions returning 'Particle's that
                 'Interact' can guarantee particle system finitude by computing their
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
