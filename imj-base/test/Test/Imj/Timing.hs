{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Timing where

import           Imj.Prelude

import           Imj.Timing

testTiming :: IO Bool
testTiming = do
  test___
  testScale
  testDuration
  testFromSecs
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

testFromSecs :: IO ()
testFromSecs = do
  let t = fromSecs (-1)
  when (toMicros t /= -1000000)
    $ error $ "t = " ++ show t

  let halfSec = fromSecs 0.5
  when (toMicros halfSec /= 500000)
    $ error $ "halfSec = " ++ show halfSec

  let oneMicros = fromSecs 0.000001
  when (toMicros oneMicros /= 1)
    $ error $ "oneMicros = " ++ show oneMicros

  let minusOneMicros = fromSecs $ -0.000001
  when (toMicros minusOneMicros /= -1)
    $ error $ "minusOneMicros = " ++ show minusOneMicros

  let oneSec = toSecs $ fromSecs 1
  unless (oneSec == 1) $ error $ "oneSec = " ++ show oneSec

  let p1Sec = toSecs $ fromSecs 0.1
  unless (p1Sec == 0.1) $ error $ "p1Sec = " ++ show p1Sec


test___ :: IO ()
test___ = do
  t <- getSystemTime
  let zeroDiff = t...t
  when (zeroDiff /= zeroDuration)
    $ error $ "zeroDiff = " ++ show zeroDiff

  let tNext = addDuration (fromSecs 1) t
      oneSecDiff = t...tNext
  when (oneSecDiff /= fromSecs 1)
    $ error $ "oneSecDiff = " ++ show oneSecDiff

  let tBefore = addDuration (fromSecs (-1)) tNext
      zeroDiff' = t...tBefore
  when (zeroDiff' /= zeroDuration)
    $ error $ "zeroDiff' = " ++ show zeroDiff'

  let tP9 = addDuration (fromSecs (-0.1)) tNext
      p9Diff = t...tP9
  when (p9Diff /= fromSecs 0.9)
    $ error $ "p9Diff = " ++ show p9Diff

  let tP1 = addDuration (fromSecs (-1.1)) tNext
      p1Diff = t...tP1
  when (p1Diff /= fromSecs (-0.1))
    $ error $ "p1Diff = " ++ show p1Diff

testScale :: IO ()
testScale = do
  let dur2 = 0.5 .* fromSecs 1.5
  unless (toSecs dur2 == 0.75) $ error $ "dur2 = " ++ show dur2

assertAlmostEq :: Double -> Double -> String -> IO ()
assertAlmostEq v1 v2 str =
  unless (abs (v1-v2) < 1e-12) $ error $ str ++ " difference : " ++ show (v1,v2)

testDuration :: IO ()
testDuration = do
  let d1 = fromSecs 1.5
      d2 = fromSecs $ -0.1
      diff  = d1 - d2
      diff' = d2 - d1
      sum_ = d1 + d2
      sum_' = d2 + d1
      d2' = negate d2
      d2'' = abs d2
      m1 = d1 * d2
      m1' = d2 * d1
  unless (toSecs diff == 1.6) $ error $ "diff = " ++ show diff
  unless (toSecs diff' == (-1.6)) $ error $ "diff' = " ++ show diff'
  unless (toSecs sum_ == 1.4) $ error $ "sum_ = " ++ show sum_
  unless (toSecs sum_' == 1.4) $ error $ "sum_' = " ++ show sum_'
  unless (toSecs d2' == 0.1) $ error $ "d2' = " ++ show d2'
  unless (toSecs d2'' == 0.1) $ error $ "d2'' = " ++ show d2''
  assertAlmostEq (toSecs m1) (-0.15) "m1"
  assertAlmostEq (toSecs m1') (-0.15) "m1'"
