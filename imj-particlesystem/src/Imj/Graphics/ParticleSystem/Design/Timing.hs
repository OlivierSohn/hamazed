{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Design.Timing
    ( particleSystemPeriod
    ) where

import           Imj.Prelude

-- | The console can refresh at approx. 21 fps, hence this value seems
-- appropriate (1/25 second)
particleSystemPeriod :: Float
particleSystemPeriod = 0.04
