{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Design.Create
    ( mkSystem
    ) where


import           Imj.Prelude

import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Graphics.ParticleSystem.Design.Update
import           Imj.Graphics.ParticleSystem.Design.UpdateParticles
import           Imj.Iteration
import           Imj.Timing

-- | Creates a 'ParticleSystem' and initializes it by updating it once.
mkSystem :: VecPosSpeed
            -- ^ Center of the root 'ParticleTree'.
            -> [VecPosSpeed -> Frame -> [Particle]]
            {- ^ /Particle functions/: each of them generates
            'Particle's for a given level of the root 'ParticleTree':

            The k-th /particle function/ generates 'ParticleTree'
            living in the (k+2)-th level, and takes the center of a (k+1)-th
            level 'ParticleTree' in its 'Coords' argument.

            /Particle functions/ are expected to return the same number of 'Particle's
            at each iteration, or none after a given iteration to indicate that
            their production is over. -}
            -> Speed
            -- ^ ParticleSystem discrete speed. Tells by how much the 'Frame', passed
            -- to particle functions, is incremented during an update.
            -> EnvFunctions
            -- ^ Functions determining when 'Particle's should be mutated,
            -- or even removed from the 'ParticleSystem'
            -> Time Point ParticleSyst
            -- ^ Starting time of the 'ParticleSystem'
            -> Maybe ParticleSystem
            -- ^ Depending on /particle functions/, the created 'ParticleSystem' may be over
            -- after the first update, hence 'Nothing' would be returned.
mkSystem center updates speed interaction t =
  let update = updateParticles updates
      u = UpdateSpec t (zeroIteration speed)
      points = mkParticleTree center
  in updateParticleSystem t $ ParticleSystem points update interaction u


-- | Constructs a 'ParticleTree'.
mkParticleTree :: VecPosSpeed
            -- ^ Where the first animation should start.
            -> ParticleTree
mkParticleTree center =
  ParticleTree Nothing center 0
