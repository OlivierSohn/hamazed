{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules exports types and functions related to timing.

module Imj.Timing
    ( -- * KeyTime
    {- | A wrapper type on 'SystemTime' -}
      KeyTime(..)
    , addDuration
    -- * SystemTime / DiffTime utilities
    , addToSystemTime
    , diffSystemTime
    , diffTimeSecToMicros
    , floatSecondsToDiffTime
    -- * Reexports
    , SystemTime(..)
    , DiffTime
    , getSystemTime
    ) where

import           Imj.Prelude
import           Prelude(Integer)

import           Data.Int(Int64)
import           Data.Time(DiffTime, diffTimeToPicoseconds,
                           secondsToDiffTime, picosecondsToDiffTime)
import           Data.Time.Clock.System
                          (getSystemTime, SystemTime(..) )

-- | Adds a 'DiffTime' to a 'SystemTime'
addToSystemTime :: DiffTime -> SystemTime -> SystemTime
addToSystemTime diff t =
  let d = diffTimeToSystemTime diff
  in sumSystemTimes d t

-- | Returns t1-t2
diffSystemTime :: SystemTime
               -- ^ t1
               -> SystemTime
               -- ^ t2
               -> DiffTime
diffSystemTime (MkSystemTime s1 ns1) (MkSystemTime s2 ns2) =
  let -- ns1 and ns2 are Word32, which is an unsigned type.
      -- To avoid underflowing Word32, to compute their difference, we use
      -- the next bigger signed type : Int64.
      ns1', ns2' :: Int64
      ns1' = fromIntegral ns1
      ns2' = fromIntegral ns2
      nsDiff = ns1' - ns2'
  in secondsToDiffTime (fromIntegral $ s1 - s2) +
     picosecondsToDiffTime (fromIntegral nsDiff * 1000)

sumSystemTimes :: SystemTime -> SystemTime -> SystemTime
sumSystemTimes (MkSystemTime s1 ns1) (MkSystemTime s2 ns2) =
  let s = s1 + s2
      ns = ns1 + ns2 -- no overflow, even if both contain leap seconds because 2^32 > 4 * 1000000000
      (addS, nanoseconds) = ns `quotRem` 1000000000
  in MkSystemTime (s + fromIntegral addS) nanoseconds


picoToNano :: Integer -> Integer
picoToNano i = quot i 1000

diffTimeToSystemTime :: DiffTime -> SystemTime
diffTimeToSystemTime diff =
  let nanoDiff :: Integer
      nanoDiff = picoToNano $ diffTimeToPicoseconds diff
      -- using divMod with a positive divisor,
      -- nanoseconds is guaranteed to be positive, seconds may be negative
      (seconds, nanoseconds) = nanoDiff `divMod` 1000000000
  in MkSystemTime (fromIntegral seconds) (fromIntegral nanoseconds)

-- | Represents deadlines and event times.
newtype KeyTime = KeyTime SystemTime deriving(Eq, Ord, Show)

-- | Convert a 'DiffTime' to a number of microseconds.
diffTimeSecToMicros :: DiffTime -> Int
diffTimeSecToMicros t = floor (t * 10^(6 :: Int))

microSecondsPerSecond :: Integer
microSecondsPerSecond = 1000000

-- | Converts a duration expressed in seconds using a 'Float' to a 'DiffTime'
--  which has picosecond resolution.
floatSecondsToDiffTime :: Float -> DiffTime
floatSecondsToDiffTime f = microsecondsToDiffTime $ floor (f*fromIntegral microSecondsPerSecond)

microsecondsToDiffTime :: Integer -> DiffTime
microsecondsToDiffTime x = fromRational (x % fromIntegral microSecondsPerSecond)

-- | Adds a 'DiffTime' to a 'KeyTime'.
addDuration :: DiffTime -> KeyTime -> KeyTime
addDuration durationSeconds (KeyTime t) = KeyTime $ addToSystemTime durationSeconds t
