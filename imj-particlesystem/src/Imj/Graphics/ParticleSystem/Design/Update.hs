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


-- | Note that if the 'ParticleSystem' deadlines become overdue,
-- there is no mechanism to "catch up", i.e we don't use the current time
-- as a reference for the new deadline, instead we use the previous deadline.
updateParticleSystem :: ParticleSystem
                     -> Maybe ParticleSystem
                     -- ^ Nothing if every contained 'Particle' is inactive.
updateParticleSystem
 (ParticleSystem points update interaction (UpdateSpec k it@(Iteration _ frame))) =
  let newPoints = update interaction frame points
  in if hasActiveParticles newPoints
       then
         Just $ ParticleSystem newPoints update interaction
              $ UpdateSpec (addDuration particleSystemPeriod k) (nextIteration it)
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
