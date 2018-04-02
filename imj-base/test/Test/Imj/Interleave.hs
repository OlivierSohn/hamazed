{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Interleave
         ( testInterleaveHalves
         ) where

import           Imj.Prelude
import           Prelude(putStrLn)
import           Data.List(elem,splitAt)

import           Imj.Geo.Discrete.Interleave
import           Imj.Graphics.Text.Render

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual

testInterleaveHalves :: IO ()
testInterleaveHalves = do
  --------------------------------------------------------------------------
  interleaveHalves [0,1::Int] `shouldBe` [0,1]

  interleaveHalves [0,1,2::Int] `shouldBe` [2,0,1]
  interleaveHalves [2,0,1::Int] `shouldBe` [1,2,0]
  interleaveHalves [1,2,0::Int] `shouldBe` [0,1,2]

  interleaveHalves [0,1,2,3::Int] `shouldBe` [2,0,3,1]
  interleaveHalves [2,0,3,1::Int] `shouldBe` [3,2,1,0]

  interleaveHalves [0,1,2,3,4::Int] `shouldBe` [4,2,0,3,1]
  interleaveHalves [4,2,0,3,1::Int] `shouldBe` [1,0,4,3,2]
  interleaveHalves [1,0,4,3,2::Int] `shouldBe` [2,4,1,3,0]
  interleaveHalves [2,4,1,3,0::Int] `shouldBe` [0,1,2,3,4]

  interleaveHalves [0,1,2,3,4,5::Int] `shouldBe` [4,2,0,5,3,1]
  interleaveHalves [4,2,0,5,3,1::Int] `shouldBe` [3,0,4,1,5,2]
  interleaveHalves [3,0,4,1,5,2::Int] `shouldBe` [5,4,3,2,1,0]

  interleaveHalves [0,1,2,3,4,5,6::Int] `shouldBe` [6,4,2,0,5,3,1]
  interleaveHalves [6,4,2,0,5,3,1::Int] `shouldBe` [1,5,2,6,3,0,4]
  interleaveHalves [1,5,2,6,3,0,4::Int] `shouldBe` [4,3,2,1,0,6,5]
  interleaveHalves [4,3,2,1,0,6,5::Int] `shouldBe` [5,0,2,4,6,1,3]
  interleaveHalves [5,0,2,4,6,1,3::Int] `shouldBe` [3,6,2,5,1,4,0]
  interleaveHalves [3,6,2,5,1,4,0::Int] `shouldBe` [0,1,2,3,4,5,6]

  interleaveHalves [0,1,2,3,4,5,6,7::Int] `shouldBe` [6,4,2,0,7,5,3,1]
  interleaveHalves [6,4,2,0,7,5,3,1::Int] `shouldBe` [3,7,2,6,1,5,0,4]
  interleaveHalves [3,7,2,6,1,5,0,4::Int] `shouldBe` [0,1,2,3,4,5,6,7]

  let f len final = do
        let analyze i prevRes x =
              bool (Left $ equivalents x ++ prevRes) (Right i) $ elem x prevRes
            equivalents x =
              map (rotate x) [0..pred len] ++ -- it's important to have x in first position
              map (rotate $ reverse x) [0..pred len]
            rotate xs n = bs ++ as where (as, bs) = splitAt n xs
        foldM
          (\res i -> return $ either
            (\prevRes@(mostRecent:_) -> analyze i prevRes $ interleaveHalves mostRecent)
            Right
            res)
          (Left $ equivalents [0..pred len])
          [1.. 100] >>= either (fail $ "not enough iterations " ++ show len) final
  -- verify formula for countUsefulInterleavedVariations:
  mapM_
    (\len -> f len (`shouldBe` countUsefulInterleavedVariations len))
    [1..20]

  -- print values
  putStrLn ""
  mapM_
    (\len ->
      f len (\value ->
        putStrLn $
          "countUsefulInterleavedVariations " ++
          justifyR 3 (show len) ++
          " = " ++
          justifyR 3 (show (value::Int))))
    [1..100]

  --------------------------------------------------------------------------
  -- verify that interleaveHalves and interleaveHalves' are the same
  mapM_
    (\i ->
      let l = [0..i]
      in interleaveHalves l `shouldBe` interleaveHalves' l)
    [0..100::Int]
