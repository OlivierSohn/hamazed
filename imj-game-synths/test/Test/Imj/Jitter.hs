{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Imj.Jitter
          ( testThreadDelay
          ) where

import Prelude(print, putStrLn, unlines)
import Imj.Prelude
import Control.Concurrent(threadDelay)

import Imj.Timing
import Imj.Statistics

{- | Results for 'testThreadDelay':
@
Duration: 100'000 (us)
| **                                                             0.0 168.26666
| ****************                                               168.26666 336.53333
| ***************************************                        336.53333 504.8
| *****************************************                      504.8 673.06665
| *****************************************                      673.06665 841.3333
| *************************************************              841.3333 1009.6
| **********************************************************     1009.60004 1177.8667
| ************************************************************** 1177.8667 1346.1333
| ****************************************                       1346.1333 1514.3999
| ********************************************                   1514.3999 1682.6666
| *******************************************                    1682.6667 1850.9333
| ******************************                                 1850.9333 2019.2
| *************************                                      2019.2 2187.4666
| *********                                                      2187.4668 2355.7334
| *                                                              2355.7334 2524.0
@

The thread is always woken up later than specified (which is in the contract of threadDelay).

On average, it is woken up one millisecond later than specified, the worst case being 2.5 milliseconds.
-}
testThreadDelay :: IO ()
testThreadDelay =
  forM_ periods $ \p -> do
    putStrLn "---"
    print p
    t <- getSystemTime
    actual <- testPeriod p t
    let expected = map (\i -> addDuration (i .* p) t) [0..]
        differences = zipWith ((...)) expected actual
    putStrLn $ show $ histogram (map (\ v -> fromIntegral $ toMicros v :: Float) differences) 15
    putStrLn $ unlines $ map show differences
    print $ maximumMaybe differences

    -- verifies 'threadDelay' contract:
    -- "There is no guarantee that the thread will be rescheduled promptly when the delay has expired,
    -- but the thread will never continue to run earlier than specified."
    mapM_
      (\d -> when (strictlyNegative d) $ error $ "earlier " ++ show d)
      differences
 where
  periods = map fromSecs [0.1,0.01,0.001,0.0001]

testPeriod :: Time Duration System -> Time Point System -> IO [Time Point System]
testPeriod period startTime =
  go 1 [startTime]
 where
  go _ [] = error "logic"
  go 500 l = return $ reverse l
  go i l@(cur:_) = do
    let startToCur = startTime...cur
        curToTarget = (i .* period) |-| startToCur
    threadDelay $ fromIntegral $ toMicros curToTarget
    now <- getSystemTime
    go (i+1) (now:l)
