module Test.Imj.Util
         ( testUtils
         , testLastAbove
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
  lastAbove 0 (return . negate) (-100) (-50) >>= shouldBe $ Just (-50)
  lastAbove 0 (return . negate) 0 569 >>= shouldBe Nothing
  lastAbove 0 (return . negate) (-1) 569 >>= shouldBe $ Just (-1)
  lastAbove 0 (return . negate) (-100) 569 >>= shouldBe $ Just (-1)
  lastAbove (-42) (return . negate) (-100) 569 >>= shouldBe $ Just 41

  lastAbove False (\v -> return $ v <= 0) 0 10 >>= shouldBe $ Just 0
  lastAbove False (\v -> return $ v < 0) 0 10  >>= shouldBe Nothing
  lastAbove False (\v -> return $ v < 10) 0 10 >>= shouldBe $ Just 9
  lastAbove False (\v -> return $ v <= 10) 0 10 >>= shouldBe $ Just 10
  lastAbove False (\v -> return $ v < 5) 0 10  >>= shouldBe $ Just 4
  lastAbove False (\v -> return $ v < 50) 0 10 >>= shouldBe $ Just 10

  -- test with invalid args:

  -- inverted min / max bounds
  lastAbove False (\_ -> return True) 10 0 >>= shouldBe Nothing

  -- predicate not being decreasing:
  lastAbove False (\v -> return $ v >= 10) 0 10 >>= shouldBe $ Nothing
