module Test.Imj.Util
         ( testUtils
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
