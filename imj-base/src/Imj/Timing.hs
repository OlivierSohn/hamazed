{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules exports types and functions related to /monotonic/ timing.

module Imj.Timing
    ( -- * KeyTime
    {- | A wrapper type on 'TimeSpec' -}
      KeyTime(..)
    , addDuration
    -- * Utilities
    , zeroTime
    , getSystemTime
    , diffTimeSecToMicros
    , secondsToTimeSpec
    , showTime
    -- * Reexports
    , toNanoSecs
    , Int64
    , TimeSpec(..)
    ) where

import           Imj.Prelude
import           Prelude(length)

import           Data.Int(Int64)
import           System.Clock(TimeSpec(..), Clock(..), getTime, toNanoSecs)

-- | Represents deadlines and event times.
newtype KeyTime = KeyTime TimeSpec deriving(Eq, Ord, Show)

-- TODO use these
--data GameTime
--data SystemTime

-- | Converts a 'TimeSpec' difference to a number of microseconds.
--
-- If the difference is more than 536 seconds (2^29 / 10^6), or less than -536 seconds,
-- it will overflow the Int.
diffTimeSecToMicros :: TimeSpec -> TimeSpec -> Int64
diffTimeSecToMicros t1 t2 =
  let (TimeSpec seconds nanos) = t1 - t2
  in 10^(6::Int) * seconds + quot nanos (10^(3::Int))

-- | Converts a duration expressed in seconds using a 'Float' to a 'TimeSpec'
secondsToTimeSpec :: Float -> TimeSpec
secondsToTimeSpec f = fromIntegral $ (floor $ f * 10^(9::Int) :: Int64)

-- | Adds a 'TimeSpec' to a 'KeyTime'.
addDuration :: Float -> KeyTime -> KeyTime
addDuration dt (KeyTime t) =
  KeyTime $ (secondsToTimeSpec dt) + t

-- | Returns the time as seen by a monotonic clock.
{-# INLINE getSystemTime #-}
getSystemTime :: IO TimeSpec
getSystemTime = getTime Monotonic

showTime :: TimeSpec -> String
showTime x = rJustify $ show $ quot (toNanoSecs x) 1000
  where rJustify txt = replicate (5-length txt) ' ' ++ txt

{-# INLINE zeroTime #-}
zeroTime :: TimeSpec
zeroTime = TimeSpec 0 0
