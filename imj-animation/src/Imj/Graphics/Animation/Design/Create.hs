{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation.Design.Create
    ( mkAnimation
    ) where


import           Imj.Prelude

import           Imj.Graphics.Animation.Design.Types
import           Imj.Graphics.Animation.Design.Update
import           Imj.Graphics.Animation.Design.UpdateParticles
import           Imj.Iteration
import           Imj.Timing

-- | Creates an animation and initializes it by updating it once.
mkAnimation :: VecPosSpeed
            -- ^ Center of the root 'Particles'.
            -> [VecPosSpeed -> Frame -> [Particle]]
            {- ^ List of /particle functions/. Every /particle function/ generates
            'Particle's for a given level of the root 'Particles':

            The k-th /particle function/ generates 'Particles'
            living in the (k+2)-th level, and takes the center of a (k+1)-th
            level 'Particles' in its 'Coords' argument.

            /Particle functions/ are expected to return the same number of 'Particle's
            at each iteration, or none after a given iteration to indicate that
            their animation is over. -}
            -> Speed
            -- ^ Animation discrete speed. Tells by how much the 'Frame', passed
            -- to particle functions, is incremented during an update.
            -> EnvFunctions
            -- ^ Functions determining when 'Particle's should be mutated,
            -- or even removed from the 'Animation'
            -> Either SystemTime KeyTime
            -- ^ 'Right' 'KeyTime' of the event's deadline
            -- that triggered this animation, or 'Left' 'SystemTime'
            -- of the current time if a player action triggered this animation
            -> Maybe Animation
            -- ^ Depending on /particle functions/, the created 'Animation' may be over
            -- after the first update, hence 'Nothing' would be returned.
mkAnimation center updates speed interaction t' =
  let update = updateParticles updates
      t = either KeyTime id t'
      u = UpdateSpec t (zeroIteration speed)
      points = mkParticles center
  in updateAnimation $ Animation points update interaction u


-- | Constructs a 'Particles'.
mkParticles :: VecPosSpeed
            -- ^ Where the first animation should start.
            -> Particles
mkParticles center =
  Particles Nothing center 0
