module Test.Imj.Harmony
          ( testDescList
          ) where

import Imj.Music.Harmony

testDescList :: IO ()
testDescList = do
  shiftModDescPatternToAscList [] `shouldBe` []
  shiftModDescPatternToAscList [0] `shouldBe` [0]
  shiftModDescPatternToAscList [1, 0] `shouldBe` [0, 1]
  shiftModDescPatternToAscList [2, 1, 0] `shouldBe` [0, 1, 2]
  shiftModDescPatternToAscList [-3, 2, 1, 0] `shouldBe` [-3, 0, 1, 2]
  shiftModDescPatternToAscList [-3, -4, 2, 1, 0] `shouldBe` [-4, -3, 0, 1, 2]

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
