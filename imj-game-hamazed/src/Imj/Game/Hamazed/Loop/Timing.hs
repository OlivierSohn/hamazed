{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

-- | This module handles time constants of the game logic.

module Imj.Game.Hamazed.Loop.Timing
        ( gameMotionPeriod
        , gameTimePointToSystemTimePoint
        , systemTimePointToGameTimePoint
        , particleSystemDurationToSystemDuration
        , particleSystemTimePointToSystemTimePoint
        , systemTimePointToParticleSystemTimePoint
        , GameTime
        -- reexport
        , module Imj.Timing
        ) where

import           Imj.Prelude

import           Imj.Graphics.ParticleSystem.Design.Timing
import           Imj.Timing

{-# INLINE gameMotionPeriod #-}
gameMotionPeriod :: Time Duration GameTime
gameMotionPeriod = fromSecs 0.16

{-# INLINE systemTimePointToGameTimePoint #-}
systemTimePointToGameTimePoint :: Time Point System -> Time Point GameTime
systemTimePointToGameTimePoint = unsafeFromTimeSpec . unsafeGetTimeSpec

{-# INLINE gameTimePointToSystemTimePoint #-}
gameTimePointToSystemTimePoint :: Time Point GameTime -> Time Point System
gameTimePointToSystemTimePoint = unsafeFromTimeSpec . unsafeGetTimeSpec

data GameTime

{-# INLINE particleSystemDurationToSystemDuration #-}
particleSystemDurationToSystemDuration :: Time Duration ParticleSyst -> Time Duration System
particleSystemDurationToSystemDuration = unsafeFromTimeSpec . unsafeGetTimeSpec

{-# INLINE particleSystemTimePointToSystemTimePoint #-}
particleSystemTimePointToSystemTimePoint :: Time Point ParticleSyst -> Time Point System
particleSystemTimePointToSystemTimePoint = unsafeFromTimeSpec . unsafeGetTimeSpec

{-# INLINE systemTimePointToParticleSystemTimePoint #-}
systemTimePointToParticleSystemTimePoint :: Time Point System -> Time Point ParticleSyst
systemTimePointToParticleSystemTimePoint = unsafeFromTimeSpec . unsafeGetTimeSpec
