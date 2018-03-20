module Test.Imj.Util
         ( testUtils
         , testLastAbove
         , testInterleaveHalves
         ) where

import           Imj.Util

testUtils :: IO ()
testUtils = do
  mkGroups 5 ([]::[Int]) `shouldBe` [[],[],[],[],[]]
  mkGroups 5 [1::Int] `shouldBe` [[1],[],[],[],[]]
  mkGroups 6 [1,2,3,4,5,6::Int] `shouldBe` [[1],[2],[3],[4],[5],[6]]
  mkGroups 8 [1,2,3,4,5,6::Int] `shouldBe` [[1],[2],[3],[4],[5],[6],[],[]]
  mkGroups 1 [1,2,3,4,5,6::Int] `shouldBe` [[1,2,3,4,5,6]]
  mkGroups 3 [1,2,3,4,5,6::Int] `shouldBe` [[1,2],[3,4],[5,6]]
  mkGroups 4 [1,2,3,4,5,6::Int] `shouldBe` [[1,2],[3,4],[5],[6]]

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
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
  lastAbove False (\v -> return $ v < 10) 0 (10 :: Int) >>= shouldBe $ Just 9
  lastAbove False (\v -> return $ v <= 10) 0 (10 :: Int) >>= shouldBe $ Just 10
  lastAbove False (\v -> return $ v < 5) 0 (10 :: Int)  >>= shouldBe $ Just 4
  lastAbove False (\v -> return $ v < 50) 0 (10 :: Int) >>= shouldBe $ Just 10

  -- test with invalid args:

  -- inverted min / max bounds
  lastAbove False (\_ -> return True) 10 (0 :: Int) >>= shouldBe Nothing

  -- predicate not being decreasing:
  lastAbove False (\v -> return $ v >= (10 :: Int)) 0 10 >>= shouldBe $ Nothing

testInterleaveHalves :: IO ()
testInterleaveHalves = do
  interleaveHalves [0,1,2,3::Int] `shouldBe` [2,0,3,1]
  interleaveHalves [2,0,3,1::Int] `shouldBe` [3,2,1,0]

  interleaveHalves [0,1,2,3,4::Int] `shouldBe` [4,2,0,3,1]
  interleaveHalves [4,2,0,3,1::Int] `shouldBe` [1,0,4,3,2]
  interleaveHalves [1,0,4,3,2::Int] `shouldBe` [2,4,1,3,0]
  interleaveHalves [2,4,1,3,0::Int] `shouldBe` [0,1,2,3,4]
