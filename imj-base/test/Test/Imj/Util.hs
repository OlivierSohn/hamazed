{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Util
         ( testUtils
         , testLastAbove
         , testLogBase2
         ) where

import           Imj.Prelude

import           Imj.Util

import           Imj.Data.AlmostFloat

testUtils :: IO ()
testUtils = do
  testMapRange

  mkGroups 5 ([]::[Int]) `shouldBe` [[],[],[],[],[]]
  mkGroups 5 [1::Int] `shouldBe` [[1],[],[],[],[]]
  mkGroups 6 [1,2,3,4,5,6::Int] `shouldBe` [[1],[2],[3],[4],[5],[6]]
  mkGroups 8 [1,2,3,4,5,6::Int] `shouldBe` [[1],[2],[3],[4],[5],[6],[],[]]
  mkGroups 1 [1,2,3,4,5,6::Int] `shouldBe` [[1,2,3,4,5,6]]
  mkGroups 3 [1,2,3,4,5,6::Int] `shouldBe` [[1,2],[3,4],[5,6]]
  mkGroups 4 [1,2,3,4,5,6::Int] `shouldBe` [[1,2],[3,4],[5],[6]]

testMapRange :: IO ()
testMapRange = do
  fromMaybe (error "test") (mapRange 0   1   0    10 0.5) `shouldBeAlmost` 5
  fromMaybe (error "test") (mapRange 0.5 1   0    10 0.5) `shouldBeAlmost` 0
  fromMaybe (error "test") (mapRange 0   0.5 0    10 0.5) `shouldBeAlmost` 10

  fromMaybe (error "test") (mapRange 0   1   (-5) 5 0.5) `shouldBeAlmost` 0
  fromMaybe (error "test") (mapRange 0   1   (-5) 5 0)   `shouldBeAlmost` (-5)
  fromMaybe (error "test") (mapRange 0   1   (-5) 5 1)   `shouldBeAlmost` 5
  mapRange (1 :: Double) 1 2 1 2 `shouldBe` Nothing


testLogBase2 :: IO ()
testLogBase2 = do
  logBase2 8 `shouldBe` 3
  logBase2 7 `shouldBe` 2

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual

shouldBeAlmost :: Float -> Float -> IO ()
shouldBeAlmost actual expected =
  if almost actual == almost expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual

testLastAbove :: IO ()
testLastAbove = do
  lastAbove 0 (return . negate) (-100) (-50 :: Int) >>= shouldBe $ Just (-50)
  lastAbove 0 (return . negate) 0 (569 :: Int) >>= shouldBe Nothing
  lastAbove 0 (return . negate) (-1) (569 :: Int) >>= shouldBe $ Just (-1)
  lastAbove 0 (return . negate) (-100) (569 :: Int) >>= shouldBe $ Just (-1)
  lastAbove (-42) (return . negate) (-100) (569 :: Int) >>= shouldBe $ Just 41

  lastAbove False (\v -> return $ v <= 0) 0 (10 :: Int) >>= shouldBe $ Just 0
  lastAbove False (\v -> return $ v < 0) 0 (10 :: Int)  >>= shouldBe Nothing
  lastAbove False (\v -> return $ v < 10) 0 (10 :: Int) >>= shouldBe $ Just 9
  lastAbove False (\v -> return $ v <= 10) 0 (10 :: Int) >>= shouldBe $ Just 10
  lastAbove False (\v -> return $ v < 5) 0 (10 :: Int)  >>= shouldBe $ Just 4
  lastAbove False (\v -> return $ v < 50) 0 (10 :: Int) >>= shouldBe $ Just 10

  -- test with invalid args:

  -- inverted min / max bounds
  lastAbove False (\_ -> return True) 10 (0 :: Int) >>= shouldBe Nothing

  -- predicate not being decreasing:
  lastAbove False (\v -> return $ v >= (10 :: Int)) 0 10 >>= shouldBe $ Nothing
