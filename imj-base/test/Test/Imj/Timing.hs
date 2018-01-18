{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Timing where

import           Imj.Prelude

import           Imj.Timing

testTiming :: IO Bool
testTiming = do
  testDiffTimeSecToMicros
  testFloatSecondsToDiffTime
  testAddSystemTime
  testAddSystemTime2
  testAddSystemTime3
  testDiffSystemTime
  testDiffSystemTime2
  testDiffSystemTime3
  return True


testDiffSystemTime :: IO ()
testDiffSystemTime = do
  let s1 = TimeSpec 1 0
      s2 = TimeSpec 2 0
      d =  s1 - s2
      d' = s2 - s1
  when (d /= TimeSpec (-1) 0) $ error $ "d = " ++ show d
  when (d' /= TimeSpec 1 0) $ error $ "d' = " ++ show d'

testDiffSystemTime2 :: IO ()
testDiffSystemTime2 = do
  let s1 = TimeSpec 1 1
      s2 = TimeSpec 1 2
      d =  s1 - s2
      d' = s2 - s1
  when (d /= TimeSpec 0 (-1)) $ error $ "d = " ++ show d
  when (d' /= TimeSpec 0 1) $ error $ "d' = " ++ show d'

testDiffSystemTime3 :: IO ()
testDiffSystemTime3 = do
  let s1 = TimeSpec 1 1
      s2 = TimeSpec 2 2
      d =  s1 - s2
      d' = s2 - s1
  when (d /= TimeSpec (-1) (-1)) $ error $ "d = " ++ show d
  when (d' /= TimeSpec 1 1) $ error $ "d' = " ++ show d'

testAddSystemTime :: IO ()
testAddSystemTime = do
  let s = TimeSpec 1 0
      (TimeSpec seconds nanos) = TimeSpec 1 0 + s
      (TimeSpec seconds' nanos') = TimeSpec (-1) 0 + s
  when (seconds /= 2) $ error $ "seconds = " ++ show seconds
  when (nanos /= 0) $ error $ "nanos = " ++ show nanos
  when (seconds' /= 0) $ error $ "seconds' = " ++ show seconds'
  when (nanos' /= 0) $ error $ "nanos' = " ++ show nanos'


testAddSystemTime2 :: IO ()
testAddSystemTime2 = do
  let s = TimeSpec 1 999999999
      (TimeSpec seconds nanos) = TimeSpec 0 1 + s
      (TimeSpec seconds' nanos') = TimeSpec 0 (-1) + s
  when (seconds /= 2) $ error $ "seconds = " ++ show seconds
  when (nanos /= 0) $ error $ "nanos = " ++ show nanos
  when (seconds' /= 1) $ error $ "seconds' = " ++ show seconds'
  when (nanos' /= 999999998) $ error $ "nanos' = " ++ show nanos'

testAddSystemTime3 :: IO ()
testAddSystemTime3 = do
  let s = TimeSpec 1 999999999
      (TimeSpec seconds nanos) = TimeSpec 1 1 + s
      (TimeSpec seconds' nanos') = TimeSpec (-1) (-1) + s
  when (seconds /= 3) $ error $ "seconds = " ++ show seconds
  when (nanos /= 0) $ error $ "nanos = " ++ show nanos
  when (seconds' /= 0) $ error $ "seconds' = " ++ show seconds'
  when (nanos' /= 999999998) $ error $ "nanos' = " ++ show nanos'

testFloatSecondsToDiffTime :: IO ()
testFloatSecondsToDiffTime = do
  let t = floatSecondsToDiffTime (-1)
  when (t /= TimeSpec (-1) 0)
    $ error $ "t = " ++ show t

  let halfSec = floatSecondsToDiffTime 0.5
  when (halfSec /= TimeSpec 0 500000000)
    $ error $ "halfSecAsMicros = " ++ show halfSec

  let oneMicros = floatSecondsToDiffTime 0.000001
  when (oneMicros /= TimeSpec 0 1000)
    $ error $ "oneMicros = " ++ show oneMicros

  let minusOneMicros = floatSecondsToDiffTime $ -0.000001
  when (minusOneMicros /= TimeSpec 0 (-1000))
    $ error $ "minusOneMicros = " ++ show minusOneMicros


testDiffTimeSecToMicros :: IO ()
testDiffTimeSecToMicros = do
  let oneSecondAsMicros = diffTimeSecToMicros (TimeSpec 1 0) (TimeSpec 0 0)
  when (oneSecondAsMicros /= 1000000)
    $ error $ "oneSecondAsMicros = " ++ show oneSecondAsMicros

  t <- getSystemTime
  let zeroDiff = diffTimeSecToMicros t t
  when (zeroDiff /= 0)
    $ error $ "zeroDiff = " ++ show zeroDiff
