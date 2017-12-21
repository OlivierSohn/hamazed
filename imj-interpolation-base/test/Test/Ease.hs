
module Test.Ease (testEase) where

import Ease


testEase :: IO()
testEase = do
  putStrLn ""
  test invQuartEaseInOut
  putStrLn ""
  test quartInOut

test :: (Float -> Float) -> IO ()
test ease = mapM_ (\v -> putStrLn $ show v) $ map ease $ map (\i -> fromIntegral i / 10.0) [(0 :: Int)..10]

quartInOut :: Float -> Float
quartInOut time =
    if time < 0.5
    then        1 / 2 *  2**4 * time  * time  * time  * time
    else negate 1 / 2 * (2**4 * (time-1) * (time-1) * (time-1) * (time-1) - 2)
