{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This modules exports types and functions related to /monotonic/ timing.

module Imj.Timing
    ( -- * Time
      Time
    , System
    , Point
    , Duration
    , unsafeGetTimeSpec
    , unsafeFromTimeSpec
    , addDuration
    -- * Utilities
    , zeroDuration
    , zeroPoint
    , getSystemTime
    , fromSecs
    , toSecs
    , showTime
    , toMicros
    , (...)
    , (.*)
    -- * Reexports
    , toNanoSecs
    , Int64
    , TimeSpec(..)
    ) where

import           Imj.Prelude
import           Prelude(length)
import           GHC.Num(fromInteger)

import           Data.Int(Int64)
import           System.Clock(TimeSpec(..), Clock(..), getTime, toNanoSecs)

{- | Represents a time.

The phantom type 'a' represents the timeline: there is system time (see 'System')
 and we could have a /game/ time, which could be a slown down, or reversed time.

 The phantom type 'b' specifies the nature of the time (a point on the timeline
 or a duration)-}
newtype Time a b = Time TimeSpec deriving(Eq, Ord, Show)

unsafeGetTimeSpec :: Time a b -> TimeSpec
unsafeGetTimeSpec (Time t) = t

unsafeFromTimeSpec :: TimeSpec -> Time a b
unsafeFromTimeSpec = Time

-- | A location on a timeline.
data Point
-- | A difference between two locations on a timeline.
data Duration

-- note that Time Point a has no Num instance because adding two of them doesn't make sense,
-- and substracting makes sense but produces a different type (Time Duration)
instance Num (Time Duration a) where
  negate (Time t) = Time $ negate t
  (+) (Time t) (Time t2) = Time $ t + t2
  (*) (Time t) (Time t2) = Time $ t * t2
  abs (Time t)    = Time $ abs t
  signum (Time t) = Time $ signum t
  fromInteger     = Time . fromInteger


-- | The system time (see 'getSystemTime')
data System

-- | Produce a duration between two points.
(...) :: Time Point b -> Time Point b -> Time Duration b
Time a ... Time b = Time $ b - a

-- | Scale a duration
(.*) :: Double -> Time Duration a -> Time Duration a
scale .* t =
  fromSecs $ scale * toSecs t

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

showTime :: Time Duration System -> String
showTime (Time x) =
  rJustify $ show $ quot (toNanoSecs x) 1000
  where
    rJustify txt = replicate (5-length txt) ' ' ++ txt

{-# INLINE zeroDuration #-}
zeroDuration :: Time Duration b
zeroDuration = Time $ TimeSpec 0 0

{-# INLINE zeroPoint #-}
zeroPoint :: Time Point b
zeroPoint = Time $ TimeSpec 0 0
