{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

-- | This module handles time constants of the game logic.

module Imj.Game.Timing
        ( particleSystemDurationToSystemDuration
        , particleSystemTimePointToSystemTimePoint
        , systemTimePointToParticleSystemTimePoint
        -- reexport
        , module Imj.Timing
        ) where

import           Imj.Prelude

import           Imj.Graphics.ParticleSystem.Design.Timing
import           Imj.Timing


{-# INLINE particleSystemDurationToSystemDuration #-}
particleSystemDurationToSystemDuration :: Time Duration ParticleSyst -> Time Duration System
particleSystemDurationToSystemDuration = unsafeFromTimeSpec . unsafeGetTimeSpec

{-# INLINE particleSystemTimePointToSystemTimePoint #-}
particleSystemTimePointToSystemTimePoint :: Time Point ParticleSyst -> Time Point System
particleSystemTimePointToSystemTimePoint = unsafeFromTimeSpec . unsafeGetTimeSpec

{-# INLINE systemTimePointToParticleSystemTimePoint #-}
systemTimePointToParticleSystemTimePoint :: Time Point System -> Time Point ParticleSyst
systemTimePointToParticleSystemTimePoint = unsafeFromTimeSpec . unsafeGetTimeSpec
