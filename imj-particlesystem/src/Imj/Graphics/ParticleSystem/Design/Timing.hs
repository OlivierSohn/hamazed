{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Design.Timing
    ( particleSystemPeriod
    , ParticleSyst
    ) where

import           Imj.Timing

-- | The console can refresh at approx. 21 fps, hence this value seems
-- appropriate (1/25 second)
{-# INLINE particleSystemPeriod #-}
particleSystemPeriod :: Time Duration ParticleSyst
particleSystemPeriod = fromSecs 0.04

data ParticleSyst
