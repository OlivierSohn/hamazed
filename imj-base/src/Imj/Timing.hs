{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules exports types and functions related to /monotonic/ timing.

module Imj.Timing
    ( -- * KeyTime
    {- | A wrapper type on 'TimeSpec' -}
      KeyTime(..)
    , addDuration
    -- * Utilities
    , getSystemTime
    , diffTimeSecToMicros
    , floatSecondsToDiffTime
    -- * Reexports
    , Int64
    , TimeSpec(..)
    ) where

import           Imj.Prelude

import           Data.Int(Int64)
import           System.Clock(TimeSpec(..), Clock(..), getTime)

-- | Represents deadlines and event times.
newtype KeyTime = KeyTime TimeSpec deriving(Eq, Ord, Show)

-- | Converts a 'TimeSpec' difference to a number of microseconds.
--
-- If the difference is more than 536 seconds (2^29 / 10^6), or less than -536 seconds,
-- it will overflow the Int.
diffTimeSecToMicros :: TimeSpec -> TimeSpec -> Int64
diffTimeSecToMicros t1 t2 =
  let (TimeSpec seconds nanos) = t1 - t2
  in 10^(6::Int) * seconds + quot nanos (10^(3::Int))

-- | Converts a duration expressed in seconds using a 'Float' to a 'TimeSpec'
floatSecondsToDiffTime :: Float -> TimeSpec
floatSecondsToDiffTime f = fromIntegral $ (floor $ f * 10^(9::Int) :: Int)

-- | Adds a 'TimeSpec' to a 'KeyTime'.
addDuration :: Float -> KeyTime -> KeyTime
addDuration dt (KeyTime t) =
  KeyTime $ (floatSecondsToDiffTime dt) + t

-- | Returns the time as seen by a monotonic clock.
{-# INLINE getSystemTime #-}
getSystemTime :: IO TimeSpec
getSystemTime = getTime Monotonic
