{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Timing where

import           Imj.Prelude
import           Data.Time(secondsToDiffTime, picosecondsToDiffTime)
import           Data.Time.Clock.System(SystemTime(..))

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
  let s1 = MkSystemTime 1 0
      s2 = MkSystemTime 2 0
      d = diffSystemTime s1 s2
      d' = diffSystemTime s2 s1
  when (d /= secondsToDiffTime (-1)) $ error $ "d = " ++ show d
  when (d' /= secondsToDiffTime 1) $ error $ "d' = " ++ show d'

testDiffSystemTime2 :: IO ()
testDiffSystemTime2 = do
  let s1 = MkSystemTime 1 1
      s2 = MkSystemTime 1 2
      d = diffSystemTime s1 s2
      d' = diffSystemTime s2 s1
  when (d /= picosecondsToDiffTime (-1000)) $ error $ "d = " ++ show d
  when (d' /= picosecondsToDiffTime 1000) $ error $ "d' = " ++ show d'

testDiffSystemTime3 :: IO ()
testDiffSystemTime3 = do
  let s1 = MkSystemTime 1 1
      s2 = MkSystemTime 2 2
      d = diffSystemTime s1 s2
      d' = diffSystemTime s2 s1
  when (d /= secondsToDiffTime (-1) + picosecondsToDiffTime (-1000)) $ error $ "d = " ++ show d
  when (d' /= secondsToDiffTime 1 + picosecondsToDiffTime 1000) $ error $ "d' = " ++ show d'

testAddSystemTime :: IO ()
testAddSystemTime = do
  let s = MkSystemTime 1 0
      (MkSystemTime seconds nanos) = addSystemTime 1 s
      (MkSystemTime seconds' nanos') = addSystemTime (-1) s
  when (seconds /= 2) $ error $ "seconds = " ++ show seconds
  when (nanos /= 0) $ error $ "nanos = " ++ show nanos
  when (seconds' /= 0) $ error $ "seconds' = " ++ show seconds'
  when (nanos' /= 0) $ error $ "nanos' = " ++ show nanos'


testAddSystemTime2 :: IO ()
testAddSystemTime2 = do
  let s = MkSystemTime 1 999999999
      (MkSystemTime seconds nanos) = addSystemTime (picosecondsToDiffTime 1000) s
      (MkSystemTime seconds' nanos') = addSystemTime (picosecondsToDiffTime (-1000)) s
  when (seconds /= 2) $ error $ "seconds = " ++ show seconds
  when (nanos /= 0) $ error $ "nanos = " ++ show nanos
  when (seconds' /= 1) $ error $ "seconds' = " ++ show seconds'
  when (nanos' /= 999999998) $ error $ "nanos' = " ++ show nanos'

testAddSystemTime3 :: IO ()
testAddSystemTime3 = do
  let s = MkSystemTime 1 999999999
      (MkSystemTime seconds nanos) = addSystemTime (picosecondsToDiffTime $ 1000000000000 + 1000) s
      (MkSystemTime seconds' nanos') = addSystemTime (picosecondsToDiffTime (-(1000000000000 + 1000))) s
  when (seconds /= 3) $ error $ "seconds = " ++ show seconds
  when (nanos /= 0) $ error $ "nanos = " ++ show nanos
  when (seconds' /= 0) $ error $ "seconds' = " ++ show seconds'
  when (nanos' /= 999999998) $ error $ "nanos' = " ++ show nanos'

testFloatSecondsToDiffTime :: IO ()
testFloatSecondsToDiffTime = do
  let minusOneSecAsMicros = diffTimeSecToMicros $ floatSecondsToDiffTime (-1)
  when (minusOneSecAsMicros /= -1000000)
    $ error $ "minusOneSecAsMicros = " ++ show minusOneSecAsMicros

  let halfSecAsMicros = diffTimeSecToMicros $ floatSecondsToDiffTime 0.5
  when (halfSecAsMicros /= 500000)
    $ error $ "halfSecAsMicros = " ++ show halfSecAsMicros

  let oneMicros = diffTimeSecToMicros $ floatSecondsToDiffTime 0.000001
  when (oneMicros /= 1)
    $ error $ "oneMicros = " ++ show oneMicros

  let minusOneMicros = diffTimeSecToMicros $ floatSecondsToDiffTime $ -0.000001
  when (minusOneMicros /= -1)
    $ error $ "minusOneMicros = " ++ show oneMicros


testDiffTimeSecToMicros :: IO ()
testDiffTimeSecToMicros = do
  let oneSecondAsMicros = diffTimeSecToMicros 1
  when (oneSecondAsMicros /= 1000000)
    $ error $ "oneSecondAsMicros = " ++ show oneSecondAsMicros

  t <- getSystemTime
  let zeroDiff = diffTimeSecToMicros $ diffSystemTime t t
  when (zeroDiff /= 0)
    $ error $ "zeroDiff = " ++ show zeroDiff
