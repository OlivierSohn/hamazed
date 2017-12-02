{-# LANGUAGE NoImplicitPrelude #-}

module Timing
    ( addGameStepDuration
    , addAnimationStepDuration
    , animationPeriod
    , animationUpdateMargin
    , computeTime
    , diffTimeSecToMicros
    , Timer(..)
    , KeyTime(..)
    , floatSecondsToNominalDiffTime
    -- | reexports
    , UTCTime(..)
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

-- the console can refresh at approx. 21 fps, hence this value (1/25)
animationPeriod :: NominalDiffTime
animationPeriod = 0.04

-- When there are multiple unsynchronized animations running,
-- there could be a lot of whole scene renderings in the same 1/100th second.
-- To prevent performance degradation we allow some margin
-- to group updates
animationUpdateMargin :: NominalDiffTime
animationUpdateMargin = 0.01

gamePeriod :: NominalDiffTime
gamePeriod = fromIntegral gamePeriodMicros / 1000000

addGameStepDuration :: KeyTime -> KeyTime
addGameStepDuration = addDuration gamePeriod

addAnimationStepDuration :: KeyTime -> KeyTime
addAnimationStepDuration = addDuration animationPeriod

addDuration :: NominalDiffTime -> KeyTime -> KeyTime
addDuration durationSeconds (KeyTime t) = KeyTime $ addUTCTime durationSeconds t

-- using the "incremental" render backend, there is no flicker
-- using the "full" render backend, flicker starts at 40
gamePeriodMicros :: Int
gamePeriodMicros = gamePeriodMillis * 1000
  where
    gamePeriodMillis = 160 -- this controls the game loop frequency.
                           -- 20 seems to match screen refresh frequency
