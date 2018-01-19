{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Design.Update
    ( updateParticleSystem
    , getDeadline
    ) where


import           Imj.Prelude

import           Data.Either(partitionEithers)

import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Graphics.ParticleSystem.Design.Timing
import           Imj.Iteration
import           Imj.Timing

-- TODO monitor the drift between current time and deadline time.
-- | We chose not to base the next deadline time on the time of the previous deadline,
-- so as to leave the liberty to the user of the library to adjust timing.
updateParticleSystem :: TimeSpec
                     -- ^ Time from which we should base the next deadline on.
                     -- In most cases, this should be the current time.
                     -> ParticleSystem
                     -> Maybe ParticleSystem
                     -- ^ Nothing if every contained 'Particle' is inactive.
updateParticleSystem t
 (ParticleSystem points update interaction (UpdateSpec _ it@(Iteration _ frame))) =
  let newPoints = update interaction frame points
  in if hasActiveParticles newPoints
       then
         Just $ ParticleSystem newPoints update interaction
              $ UpdateSpec (addDuration particleSystemPeriod (KeyTime t)) (nextIteration it)
       else
         Nothing

hasActiveParticles :: ParticleTree -> Bool
hasActiveParticles (ParticleTree Nothing _ _)         = False
hasActiveParticles (ParticleTree (Just []) _ _)       = False
hasActiveParticles (ParticleTree (Just branches) _ _) =
  let (children, activeCoordinates) = partitionEithers branches
      childrenActive = map hasActiveParticles children
  in (not . null) activeCoordinates || or childrenActive

-- | Returns the time at which an 'ParticleSystem' should be updated.
getDeadline :: ParticleSystem -> KeyTime
getDeadline (ParticleSystem _ _ _ (UpdateSpec k _)) = k
