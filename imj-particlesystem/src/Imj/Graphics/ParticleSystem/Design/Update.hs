{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Design.Update
    ( shouldUpdate
    , updateParticleSystem
    , getDeadline
    ) where


import           Imj.Prelude

import           Data.Either(partitionEithers)

import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Graphics.ParticleSystem.Design.Timing
import           Imj.Iteration
import           Imj.Timing


{- | Returns 'True' if the 'KeyTime' is beyond the 'ParticleSystem' deadline or if the
time difference is within 'particleSystemUpdateMargin'. -}
shouldUpdate :: ParticleSystem
             -> KeyTime
             -- ^ The current 'KeyTime'
             -> Bool
shouldUpdate a (KeyTime k) =
  let (KeyTime k') = getDeadline a
  in diffSystemTime k' k < particleSystemUpdateMargin

updateParticleSystem :: ParticleSystem
                     -> Maybe ParticleSystem
                     -- ^ Nothing if all contained 'Particle's are inactive.
updateParticleSystem
 (ParticleSystem points update interaction (UpdateSpec k it@(Iteration _ frame))) =
  let newPoints = update interaction frame points
      newUpdateSpec = UpdateSpec (addDuration particleSystemPeriod k) (nextIteration it)
      newAnim = ParticleSystem newPoints update interaction newUpdateSpec
  in if isActive newAnim
       then
         Just newAnim
       else
         Nothing

isActive :: ParticleSystem
         -> Bool
isActive (ParticleSystem points _ _ _) =
  hasActiveParticles points

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
