{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules handle time constants of the game logic.

module Imj.Game.Hamazed.Loop.Timing
        ( gameMotionPeriod
        , module Imj.Timing
        ) where

import           Imj.Prelude

import           Imj.Timing

{-# INLINE gameMotionPeriod #-}
gameMotionPeriod :: Float
gameMotionPeriod =
  fromIntegral gameMotionPeriodMicros / 1000000

{-# INLINE gameMotionPeriodMicros #-}
-- using the "delta" render backend, there is no flicker
-- using the "naive" render backend, flicker starts at 40
gameMotionPeriodMicros :: Int
gameMotionPeriodMicros =
  millis * 1000
 where
  millis = 160 -- 20 seems to match screen refresh frequency
