{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- | This modules exports types and functions related to /monotonic/ timing.

Some functions are /unsafe/-prefixed. You should use them only to implement
conversion between different time-spaces. Otherwise, these functions are too
low-level for your usage and may lead to mistakes because you'll convert from one
time-space to another wihtout noticing it.
-}
module Imj.Timing
    ( -- * Time
      Time
    , System
    , Point
    , Duration
    , addDuration
    -- * Utilities
    , zeroDuration
    , unsafeGetTimeSpec
    , unsafeFromTimeSpec
    , getSystemTime
    , fromSecs
    , toSecs
    , showTime
    , toMicros
    , (...)
    , (.*)
    , (|-|)
    , (|+|)
    , strictlyNegative
    -- * Reexports
    , Int64
    ) where

import           Imj.Prelude
import           Prelude(length)

import           Data.Int(Int64)
import           System.Clock(TimeSpec(..), Clock(..), getTime, toNanoSecs)

{- | Represents a time.

The phantom type 'a' represents the timeline: there is system time (see 'System')
 and we could have a /game/ time, which could be a slown down, or reversed time.

 The phantom type 'b' specifies the nature of the time (a point on the timeline
 or a duration)-}
newtype Time a b = Time TimeSpec deriving(Eq, Ord, Show, Generic)
instance PrettyVal (Time Point b) where
  prettyVal (Time (TimeSpec s n)) = prettyVal ("TimePoint:", s, n)
instance PrettyVal (Time Duration b) where
  prettyVal (Time (TimeSpec s n)) = prettyVal ("Duration:", s, n)

unsafeGetTimeSpec :: Time a b -> TimeSpec
unsafeGetTimeSpec (Time t) = t

unsafeFromTimeSpec :: TimeSpec -> Time a b
unsafeFromTimeSpec = Time

{- | A location on a timeline.

Note that summing 'Time' 'Point' has no meaning, and substracting them is achieved
using '...' -}
data Point deriving(Generic)

{- | A difference between two locations on a timeline.

I prefer not to give a 'Num' to 'Time' 'Duration', because fromInteger takes nanoseconds, which
leads to ambiguous code (see <https://github.com/corsis/clock/issues/49 this issue>).

Instead, '|-|' and '|+|' are available.
-}
data Duration deriving(Generic)


{- | Substraction for 'Time' 'Duration' -}
(|-|) :: Time Duration a -> Time Duration a -> Time Duration a
Time a |-| Time b = Time $ a-b
{- | Addition for 'Time' 'Duration' -}
(|+|) :: Time Duration a -> Time Duration a -> Time Duration a
Time a |+| Time b = Time $ a+b
{- | Scalar multiplication for 'Time' 'Duration' -}
(.*) :: Double -> Time Duration a -> Time Duration a
scale .* t =
  fromSecs $ scale * toSecs t

strictlyNegative :: Time Duration a -> Bool
strictlyNegative (Time t) = t < 0

-- | The system time (see 'getSystemTime')
data System deriving(Generic)

-- | Produce a duration between two points.
(...) :: Time Point b
      -- ^ t1
      -> Time Point b
      -- ^ t2
      -> Time Duration b
      -- ^ = t2 - t1
Time a ... Time b = Time $ b - a

-- | Adds seconds to a 'Point'.
{-# INLINE addDuration #-}
addDuration :: Time Duration b -> Time Point b -> Time Point b
addDuration (Time dt) (Time t) =
  Time $ t + dt

{-# INLINE toMicros #-}
-- | Only provide this function for 'System' time durations, to call 'timeout'.
toMicros :: Time Duration System -> Int64
toMicros (Time (TimeSpec seconds nanos)) =
  10^(6::Int) * seconds + quot nanos (10^(3::Int))

toSecs :: Time Duration a -> Double
toSecs (Time (TimeSpec seconds nanos)) =
  fromIntegral seconds + fromIntegral nanos / fromIntegral (10^(9::Int) :: Int)

-- | Converts a duration expressed in seconds using a 'Double' to a 'TimeSpec'
fromSecs :: Double -> Time Duration b
fromSecs f =
  Time $ fromIntegral $ (floor $ f * 10^(9::Int) :: Int64)

-- | Returns the time as seen by a monotonic clock.
{-# INLINE getSystemTime #-}
getSystemTime :: IO (Time Point System)
getSystemTime =
  Time <$> getTime Monotonic

showTime :: Time Duration a -> String
showTime (Time x) =
  rJustify $ show $ quot (toNanoSecs x) 1000
  where
    rJustify txt = replicate (5-length txt) ' ' ++ txt

{-# INLINE zeroDuration #-}
zeroDuration :: Time Duration b
zeroDuration = Time $ TimeSpec 0 0
