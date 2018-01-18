{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Design.Timing
    ( particleSystemPeriod
    ) where

import Imj.Timing

-- | The console can refresh at approx. 21 fps, hence this value seems
-- appropriate (1/25 second)
particleSystemPeriod :: DiffTime
particleSystemPeriod = 0.04
