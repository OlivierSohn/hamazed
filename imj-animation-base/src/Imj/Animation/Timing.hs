{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules handle time constants of animations.

module Imj.Animation.Timing
    ( addAnimationStepDuration
    , animationPeriod
    , animationUpdateMargin
    , module Imj.Timing
    ) where

import Imj.Timing

-- the console can refresh at approx. 21 fps, hence this value (1/25)
animationPeriod :: DiffTime
animationPeriod = 0.04

-- When there are multiple unsynchronized animations running,
-- there could be a lot of whole scene renderings in the same 1/100th second.
-- To prevent performance degradation we allow some margin
-- to group updates
animationUpdateMargin :: DiffTime
animationUpdateMargin = 0.01


addAnimationStepDuration :: KeyTime -> KeyTime
addAnimationStepDuration = addDuration animationPeriod
