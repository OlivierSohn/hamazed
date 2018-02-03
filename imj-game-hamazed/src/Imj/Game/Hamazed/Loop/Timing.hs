{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

-- | This module handles time constants of the game logic.

module Imj.Game.Hamazed.Loop.Timing
        ( gameMotionPeriod
        , particleSystemDurationToSystemDuration
        , particleSystemTimePointToSystemTimePoint
        , systemTimePointToParticleSystemTimePoint
        , GameTime
        , initalGameMultiplicator
        -- reexport
        , module Imj.Timing
        ) where

import           Imj.Prelude

import           Imj.Graphics.ParticleSystem.Design.Timing
import           Imj.Timing

{-# INLINE gameMotionPeriod #-}
-- | Nominal game period when the associated 'Multiplicator' is 1.
gameMotionPeriod :: Time Duration GameTime
gameMotionPeriod = fromSecs 0.16

data GameTime

initalGameMultiplicator :: Multiplicator GameTime
initalGameMultiplicator = gameTimeMultiplicator 1

gameTimeMultiplicator :: Double -> Multiplicator GameTime
gameTimeMultiplicator = Multiplicator

{-# INLINE particleSystemDurationToSystemDuration #-}
particleSystemDurationToSystemDuration :: Time Duration ParticleSyst -> Time Duration System
particleSystemDurationToSystemDuration = unsafeFromTimeSpec . unsafeGetTimeSpec

{-# INLINE particleSystemTimePointToSystemTimePoint #-}
particleSystemTimePointToSystemTimePoint :: Time Point ParticleSyst -> Time Point System
particleSystemTimePointToSystemTimePoint = unsafeFromTimeSpec . unsafeGetTimeSpec

{-# INLINE systemTimePointToParticleSystemTimePoint #-}
systemTimePointToParticleSystemTimePoint :: Time Point System -> Time Point ParticleSyst
systemTimePointToParticleSystemTimePoint = unsafeFromTimeSpec . unsafeGetTimeSpec
