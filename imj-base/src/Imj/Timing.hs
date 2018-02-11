{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- | This module exports types and functions related to /monotonic/ timing.

Some functions are /unsafe/-prefixed. You should use them only to implement
conversion between different time-spaces (time-space = first phantom type of 'Time').
Otherwise, these functions are too
low-level for your usage and may lead to mistakes because you'll convert from one
time-space to another wihtout noticing it.
-}
module Imj.Timing
    ( -- * Time
      Time
    , System
    , Point
    , Duration
    -- * Translate a time point
    , addDuration
    -- * Produce durations
    , (...)
    -- * Scale, add, substract durations
    , (.*)
    , (|-|)
    , (|+|)
    -- * Convert durations between time spaces
    , Multiplicator(..)
    , fromSystemDuration
    , toSystemDuration
    -- * Convert system durations from / to seconds
    , fromSecs
    , unsafeToSecs
    -- * Utilities
    , getSystemTime
    , getDurationFromNowTo
    , showTime
    , toMicros
    , strictlyNegative
    , zeroDuration
    , unsafeGetTimeSpec
    , unsafeFromTimeSpec
    -- * Reexports
    , Int64
    ) where

import           Imj.Prelude
import           Prelude(length)
import           Control.DeepSeq(NFData(..))
import           Data.Int(Int64)
import           System.Clock(TimeSpec(..), Clock(..), getTime, toNanoSecs)

{- | Represents a time.

The phantom type 'a' represents the time space. It could be 'System'
 or another phantom type.

 The phantom type 'b' specifies the nature of the time (a point on the timeline
 or a duration)-}
newtype Time a b = Time TimeSpec deriving(Generic, Eq, Ord, Show)
instance NFData (Time a b) where
  rnf _ = () -- TimeSpec has unboxed fields so they are already in nf


instance PrettyVal (Time Point b) where
  prettyVal (Time (TimeSpec s n)) = prettyVal ("TimePoint:", s, n)
instance PrettyVal (Time Duration b) where
  prettyVal (Time (TimeSpec s n)) = prettyVal ("Duration:", s, n)

{- | A location on a timeline.

Note that summing 'Time' 'Point' has no meaning, and substracting them is achieved
using '...' -}
data Point deriving(Generic) -- do not serialize time points as it doesn't make much sense.

{- | A difference between two locations of the same time space.

See 'Multiplicator', 'fromSystemDuration' and 'toSystemDuration' to convert
a 'Duration' of a given time space from / to the 'SystemTime' time space.

'|-|' and '|+|' are available to add or substract durations.
A 'Num' instance is /not/ provided, as it would lead to more
ambiguous code, as explained <https://github.com/corsis/clock/issues/49 here>. -}
data Duration deriving(Generic, Binary)

-- | The system time (see 'getSystemTime')
data System deriving(Generic, Binary)

-- | 'Multiplicator', multiplied with a 'System' duration produces a duration in
-- another time space specified by the phantom type 'a'.
newtype Multiplicator a = Multiplicator Double deriving(Eq, Show, Generic, PrettyVal, NFData)


{-# INLINE fromSystemDuration #-}
fromSystemDuration :: Multiplicator a -> Time Duration System -> Time Duration a
fromSystemDuration (Multiplicator m) = unsafeFromTimeSpec . unsafeGetTimeSpec . ((.*) m)

{-# INLINE toSystemDuration #-}
toSystemDuration :: Multiplicator a -> Time Duration a -> Time Duration System
toSystemDuration (Multiplicator m) = ((.*) $ recip m) . unsafeFromTimeSpec . unsafeGetTimeSpec


{- | Substraction for 'Time' 'Duration' -}
(|-|) :: Time Duration a -> Time Duration a -> Time Duration a
Time a |-| Time b = Time $ a-b
{- | Addition for 'Time' 'Duration' -}
(|+|) :: Time Duration a -> Time Duration a -> Time Duration a
Time a |+| Time b = Time $ a+b
{- | Scalar multiplication for 'Time' 'Duration' -}
(.*) :: Double -> Time Duration a -> Time Duration a
scale .* t =
  fromSecs $ scale * unsafeToSecs t
{-# INLINE (.*) #-}
{-# INLINE (|-|) #-}
{-# INLINE (|+|) #-}

-- | Produce a duration between two points.
{-# INLINE (...) #-}
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

{-# INLINE unsafeToSecs #-}
unsafeToSecs :: Time Duration a -> Double
unsafeToSecs (Time (TimeSpec seconds nanos)) =
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

getDurationFromNowTo :: Time Point System
                     -> IO (Time Duration System)
getDurationFromNowTo t =
  getSystemTime >>= \now -> return $ now ... t

showTime :: Time Duration a -> String
showTime (Time x) =
  rJustify $ show $ quot (toNanoSecs x) 1000
  where
    rJustify txt = replicate (5-length txt) ' ' ++ txt

{-# INLINE zeroDuration #-}
zeroDuration :: Time Duration b
zeroDuration = Time $ TimeSpec 0 0

{-# INLINE unsafeGetTimeSpec #-}
unsafeGetTimeSpec :: Time a b -> TimeSpec
unsafeGetTimeSpec (Time t) = t

{-# INLINE unsafeFromTimeSpec #-}
unsafeFromTimeSpec :: TimeSpec -> Time a b
unsafeFromTimeSpec = Time

{-# INLINE strictlyNegative #-}
strictlyNegative :: Time Duration a -> Bool
strictlyNegative (Time t) = t < 0
