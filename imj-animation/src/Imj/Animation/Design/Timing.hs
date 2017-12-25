{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Design.Timing
    ( animationPeriod
    , animationUpdateMargin
    ) where

import Imj.Timing

-- | The console can refresh at approx. 21 fps, hence this value seems
-- appropriate (1/25 second)
animationPeriod :: DiffTime
animationPeriod = 0.04

{- | When there are multiple unsynchronized animations running,
there could be a lot of scene renderings in the same 1/100th second.

To prevent performance degradation we allow some margin
to group update deadlines.
-}
animationUpdateMargin :: DiffTime
animationUpdateMargin = 0.01
