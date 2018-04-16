{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Quantifiable(testAverageable) where

import           Imj.Prelude

import           Imj.Data.AlmostFloat
import           Imj.Timing
import           Imj.Data.Class.Quantifiable

testAverageable :: IO ()
testAverageable = do
  average [0,1,2,4,5,6] `shouldBeAlmost` (3 :: Float)
  average (map fromSecs [0,1,2,4,5,6]) `shouldBeAlmost` fromSecs 3
  average [] `shouldBe` (0 :: Float)

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
