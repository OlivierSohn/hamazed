
module Timing
    ( addGameStepDuration
    , addAnimationStepDuration
    , animationSpeed
    , computeTime
    , diffTimeSecToMicros
    , nextUpdateCounter
    , showUpdateTick
    , Timer(..)
    , KeyTime(..)
    -- | reexports
    , UTCTime(..)
    , diffUTCTime
    , addUTCTime
    , getCurrentTime
    ) where

import           Imajuscule.Prelude

import           Data.Time( addUTCTime
                          , diffUTCTime
                          , getCurrentTime
                          , NominalDiffTime
                          , UTCTime(..) )
import           Geo( Col(..)
                    , Coords(..) )
import           WorldSize( WorldSize(..) )


-- I introduce this type to prevent equality test which make no sense, like
-- between "current system time" and a time that was computed
newtype KeyTime = KeyTime UTCTime deriving(Eq, Ord, Show)

diffTimeSecToMicros :: NominalDiffTime -> Int
diffTimeSecToMicros t = floor (t * 10^(6 :: Int))


newtype Timer = Timer { _initialTime :: UTCTime }

computeTime :: Timer -> UTCTime -> Int
computeTime (Timer t1) t2 =
  let t = diffUTCTime t2 t1
  in floor t


-- the console can refresh at approx. 21 fps, hence this value (1/25)
animationPeriod :: NominalDiffTime
animationPeriod = 0.04

-- the number of increments added at each step
animationSpeed :: Int
animationSpeed = 2

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


tickRepresentationLength :: Col -> Int
tickRepresentationLength (Col c) = quot c 2


showUpdateTick :: Int -> WorldSize -> String
showUpdateTick t (WorldSize (Coords _ c@(Col cs))) =
  let l = tickRepresentationLength c
      nDotsBefore = max 0 (t + l - cs)
      nLeftBlanks = t - nDotsBefore
      nDotsAfter = l - nDotsBefore
      nRightBlanks = cs - t - l
  in replicate nDotsBefore  '.'
  ++ replicate nLeftBlanks  ' '
  ++ replicate nDotsAfter   '.'
  ++ replicate nRightBlanks ' '


nextUpdateCounter :: Col -> Int -> Int
nextUpdateCounter (Col c) i = (i + 1) `mod` c
