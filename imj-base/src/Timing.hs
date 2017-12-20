{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules exports types and functions related to timing.

module Timing
    ( -- * Types
      KeyTime(..)
    -- * Utilities
    , addDuration
    , diffTimeSecToMicros
    , floatSecondsToNominalDiffTime
    -- * Reexports
    , UTCTime(..)
    , NominalDiffTime
    , addUTCTime
    , diffUTCTime
    , getCurrentTime
    ) where

import qualified Prelude(Integer)
import           Imajuscule.Prelude

import           Data.Time( addUTCTime
                          , diffUTCTime
                          , getCurrentTime
                          , NominalDiffTime
                          , UTCTime(..) )


-- | Represents deadlines and event times.
newtype KeyTime = KeyTime UTCTime deriving(Eq, Ord, Show)

-- | Convert a 'NominalDiffTime' to a number of microseconds.
diffTimeSecToMicros :: NominalDiffTime -> Int
diffTimeSecToMicros t = floor (t * 10^(6 :: Int))

microSecondsPerSecond :: Prelude.Integer
microSecondsPerSecond = 1000000

-- | Converts a duration expressed in seconds using a 'Float' to a 'NominalDiffTime'
--  which has picosecond resolution.
floatSecondsToNominalDiffTime :: Float -> NominalDiffTime
floatSecondsToNominalDiffTime f = microsecondsToNominalDiffTime $ floor (f*fromIntegral microSecondsPerSecond)

microsecondsToNominalDiffTime :: Prelude.Integer -> NominalDiffTime
microsecondsToNominalDiffTime x = fromRational (x % fromIntegral microSecondsPerSecond)

-- | Adds a 'NominalDiffTime' to a 'KeyTime'.
addDuration :: NominalDiffTime -> KeyTime -> KeyTime
addDuration durationSeconds (KeyTime t) = KeyTime $ addUTCTime durationSeconds t
