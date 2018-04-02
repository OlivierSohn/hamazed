{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Quantifiable(testAverageable) where

import           Imj.Prelude
import           Prelude(putStrLn)

import           Test.Imj.Utilities
import           Imj.Timing
import           Imj.Data.Class.Quantifiable
import           Imj.Graphics.Text.Render

testAverageable :: IO ()
testAverageable = do
  average [0,1,2,4,5,6] `shouldBeAlmost` (3 :: Float)
  average (map fromSecs [0,1,2,4,5,6]) `shouldBeAlmost` fromSecs 3
  average [] `shouldBe` (0 :: Float)
  mapM_ putStrLn $ showQuantities [1 :: Float,2,3,4,5]
  mapM_ putStrLn $ showQuantities $ map fromSecs [1,2,3,4,5]

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual

shouldBeAlmost :: (Quantifiable a) => a -> a -> IO ()
shouldBeAlmost actual expected =
  if almost (writeFloat actual) == almost (writeFloat expected)
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
