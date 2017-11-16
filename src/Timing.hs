
module Timing
    ( addMotionStepDuration
    , computeTime
    , diffTimeSecToMicros
    , eraMicros
    , nextUpdateCounter
    , showUpdateTick
    , Timer(..)
    ) where


import           Data.Time( addUTCTime
                          , diffUTCTime
                          , NominalDiffTime
                          , UTCTime )
import           Geo( Col(..)
                    , Coords(..) )
import           WorldSize( WorldSize(..) )



diffTimeSecToMicros :: NominalDiffTime -> Int
diffTimeSecToMicros t = floor (t * 10^(6 :: Int))


newtype Timer = Timer { _initialTime :: UTCTime }


computeTime :: Timer -> UTCTime -> Int
computeTime (Timer t1) t2 =
  let t = diffUTCTime t2 t1
  in floor t


motionStepDurationSeconds :: Float
motionStepDurationSeconds = fromIntegral eraMicros / 1000000

addMotionStepDuration :: UTCTime -> UTCTime
addMotionStepDuration = addUTCTime (realToFrac motionStepDurationSeconds)

eraMicros :: Int
eraMicros = eraMillis * 1000
  where
    eraMillis = 160 -- this controls the game loop frequency.
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
