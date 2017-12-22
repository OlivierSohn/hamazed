{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Timing
    ( addAnimationStepDuration
    , animationPeriod
    , animationUpdateMargin
    , module Imj.Timing
    ) where

import Imj.Timing

-- | The console can refresh at approx. 21 fps, hence this value (1/25)
animationPeriod :: DiffTime
animationPeriod = 0.04

{- | When there are multiple unsynchronized animations running,
there could be a lot of scene renderings in the same 1/100th second.

To prevent performance degradation we allow some margin
to group update deadlines.
-}
animationUpdateMargin :: DiffTime
animationUpdateMargin = 0.01

-- | Adds the duration of one animation step.
addAnimationStepDuration :: KeyTime -> KeyTime
addAnimationStepDuration = addDuration animationPeriod
