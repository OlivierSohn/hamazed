{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.CyclicMatrix
           ( testCyclicMatrix
           ) where

import           Imj.Prelude
import           Prelude(print, take, putStrLn)

import           Imj.Data.Matrix.Cyclic
import           Imj.Geo.Discrete.Interleave

testCyclicMatrix :: IO ()
testCyclicMatrix = do
  testRotations
  testModulate
  testInterleave

testRotations :: IO ()
testRotations = do
  let m = fromList 3 5 $ take 30 [0 :: Int ..]
  mapM_ print $ produceRotations Order1 m

testModulate :: IO ()
testModulate = do
  let m6 = fromList 1 6 [0 :: Int ..5]
  modulate 1 m6 `shouldBe` m6
  modulate 2 m6 `shouldBe` fromList 1 6 [0,2,4,1,3,5 :: Int]
  modulate 3 m6 `shouldBe` fromList 1 6 [0,3,1,4,2,5 :: Int]
  modulate 4 m6 `shouldBe` fromList 1 6 [0,4,1,5,2,3 :: Int]
  modulate 5 m6 `shouldBe` fromList 1 6 [0,5,1,2,3,4 :: Int]

testInterleave :: IO ()
testInterleave = do
  let nRows = 9
      nCols = 18
      m = matrix nRows nCols $ \rIdx _ c -> rIdx + c
  putStrLn "interleave"
  print m
  mapM_ print $ produceUsefulInterleavedVariations (mkInterleaveInfo nRows) (mkInterleaveInfo nCols) m


shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
