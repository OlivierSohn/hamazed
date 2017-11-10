
module Timing
    ( computeTime
    , eraMicros
    , nextUpdateCounter
    , showUpdateTick
    , Timer(..)
    ) where


import           Data.Time( UTCTime
                          , diffUTCTime )
import           World( worldSize )


newtype Timer = Timer { _initialTime :: UTCTime }


computeTime :: Timer -> UTCTime -> Int
computeTime (Timer t1) t2 =
  let t = diffUTCTime t2 t1
  in floor t



eraMicros :: Int
eraMicros = eraMillis * 1000
  where
    eraMillis = 160 -- this controls the game loop frequency.
                    -- 20 seems to match screen refresh frequency

maxUpdateTick :: Int
maxUpdateTick = worldSize


tickRepresentationLength :: Int
tickRepresentationLength = quot maxUpdateTick 2


showUpdateTick :: Int -> String
showUpdateTick t =
  let nDotsBefore = max 0 (t + tickRepresentationLength - maxUpdateTick)
      nLEFTBlanks = t - nDotsBefore
      nDotsAfter = tickRepresentationLength - nDotsBefore
      nRIGHTBlanks = maxUpdateTick - t - tickRepresentationLength
  in replicate nDotsBefore  '.'
  ++ replicate nLEFTBlanks  ' '
  ++ replicate nDotsAfter   '.'
  ++ replicate nRIGHTBlanks ' '


nextUpdateCounter :: Int -> Int
nextUpdateCounter c = (c + 1) `mod` maxUpdateTick
