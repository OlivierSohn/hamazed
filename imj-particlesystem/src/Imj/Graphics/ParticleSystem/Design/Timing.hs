{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Design.Timing
    ( particleSystemPeriod
    , particleSystemUpdateMargin
    ) where

import Imj.Timing

-- | The console can refresh at approx. 21 fps, hence this value seems
-- appropriate (1/25 second)
particleSystemPeriod :: DiffTime
particleSystemPeriod = 0.04

{- | When there are multiple unsynchronized particle systems running,
there could be a lot of scene renderings in the same 1/100th second.

To prevent performance degradation we allow some margin
to group deadlines.
-}
particleSystemUpdateMargin :: DiffTime
particleSystemUpdateMargin = 0.01
