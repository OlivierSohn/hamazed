{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules handle time constants of animations and game.

module Timing
    ( addDuration
    , computeTime
    , diffTimeSecToMicros
    , Timer(..)
    , KeyTime(..)
    , floatSecondsToNominalDiffTime
    -- | reexports
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


-- I introduce this type to prevent equality test which make no sense, like
-- between "current system time" and a time that was computed
newtype KeyTime = KeyTime UTCTime deriving(Eq, Ord, Show)

diffTimeSecToMicros :: NominalDiffTime -> Int
diffTimeSecToMicros t = floor (t * 10^(6 :: Int))

microSecondsPerSecond :: Prelude.Integer
microSecondsPerSecond = 1000000

floatSecondsToNominalDiffTime :: Float -> NominalDiffTime
floatSecondsToNominalDiffTime f = microsecondsToNominalDiffTime $ floor (f*fromIntegral microSecondsPerSecond)

microsecondsToNominalDiffTime :: Prelude.Integer -> NominalDiffTime
microsecondsToNominalDiffTime x = fromRational (x % fromIntegral microSecondsPerSecond)


newtype Timer = Timer { _initialTime :: UTCTime }

computeTime :: Timer -> UTCTime -> Int
computeTime (Timer t1) t2 =
  let t = diffUTCTime t2 t1
  in floor t

addDuration :: NominalDiffTime -> KeyTime -> KeyTime
addDuration durationSeconds (KeyTime t) = KeyTime $ addUTCTime durationSeconds t
