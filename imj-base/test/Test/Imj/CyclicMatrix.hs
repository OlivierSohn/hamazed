{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.CyclicMatrix
           ( testCyclicMatrix
           ) where

import           Imj.Prelude
import           Prelude(print, take)
import           Imj.Data.Matrix.Cyclic

testCyclicMatrix :: IO ()
testCyclicMatrix = do
  let m = fromList 3 5 $ take 30 [0 :: Int ..]
  mapM_ print $ produceRotations Order1 m

  let m6 = fromList 1 6 [0 :: Int ..5]
  modulate 1 m6 `shouldBe` m6
  modulate 2 m6 `shouldBe` fromList 1 6 [0,2,4,1,3,5 :: Int]
  modulate 3 m6 `shouldBe` fromList 1 6 [0,3,1,4,2,5 :: Int]
  modulate 4 m6 `shouldBe` fromList 1 6 [0,4,1,5,2,3 :: Int]
  modulate 5 m6 `shouldBe` fromList 1 6 [0,5,1,2,3,4 :: Int]


shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
