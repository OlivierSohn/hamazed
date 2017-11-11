
module Timing
    ( addMotionStepDuration
    , computeTime
    , eraMicros
    , nextUpdateCounter
    , showUpdateTick
    , Timer(..)
    ) where


import           Data.Time( addUTCTime
                          , diffUTCTime
                          , UTCTime )
import           World( WorldSize(..) )


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


tickRepresentationLength :: WorldSize -> Int
tickRepresentationLength (WorldSize worldSize) = quot worldSize 2


showUpdateTick :: Int -> WorldSize -> String
showUpdateTick t ws@(WorldSize worldSize) =
  let l = tickRepresentationLength ws
      nDotsBefore = max 0 (t + l - worldSize)
      nLEFTBlanks = t - nDotsBefore
      nDotsAfter = l - nDotsBefore
      nRIGHTBlanks = worldSize - t - l
  in replicate nDotsBefore  '.'
  ++ replicate nLEFTBlanks  ' '
  ++ replicate nDotsAfter   '.'
  ++ replicate nRIGHTBlanks ' '


nextUpdateCounter :: WorldSize -> Int -> Int
nextUpdateCounter (WorldSize worldSize) c = (c + 1) `mod` worldSize
