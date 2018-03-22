{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Interleave
         ( testInterleaveHalves
         ) where

import           Imj.Prelude
import           Prelude(print)

import           Data.List(elem,splitAt)
import           Imj.Geo.Discrete.Interleave

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
  -- verify formula for countUsefullInterleavedVariations:
  mapM_
    (\len -> f len (`shouldBe` countUsefullInterleavedVariations len))
    [1..20]

  -- print values
  mapM_
    (\len -> f len (\value -> print (len,value::Int)))
    [1..100]

  --------------------------------------------------------------------------
  -- shows that interleaveHalves and interleaveHalves'' are the same
  interleaveHalves'' [0,1,2,3::Int] `shouldBe` [2,0,3,1]
  interleaveHalves'' [2,0,3,1::Int] `shouldBe` [3,2,1,0]

  interleaveHalves'' [0,1,2,3,4::Int] `shouldBe` [4,2,0,3,1]
  interleaveHalves'' [4,2,0,3,1::Int] `shouldBe` [1,0,4,3,2]
  interleaveHalves'' [1,0,4,3,2::Int] `shouldBe` [2,4,1,3,0]
  interleaveHalves'' [2,4,1,3,0::Int] `shouldBe` [0,1,2,3,4]

  --------------------------------------------------------------------------
  -- with a list of length 2, 1 variation is /useful wrt topology/:
  interleaveHalvesKeepExtremities [0,1::Int] `shouldBe` [1,0]

  -- with a list of length 3, 1 variation is /useful wrt topology/:
  interleaveHalvesKeepExtremities [0,1,2::Int] `shouldBe` [2,1,0]

  -- with a list of length 4, 2 variations are /useful wrt topology/:
  interleaveHalvesKeepExtremities [0,1,2,3::Int] `shouldBe` [3,1,2,0]
  interleaveHalvesKeepExtremities [3,1,2,0::Int] `shouldBe` [0,1,2,3]

  -- with a list of length 5, 6 variations are /useful wrt topology/:
  interleaveHalvesKeepExtremities [0,1,2,3,4::Int] `shouldBe` [4,3,1,2,0]
  interleaveHalvesKeepExtremities [4,3,1,2,0::Int] `shouldBe` [0,2,3,1,4]
  interleaveHalvesKeepExtremities [0,2,3,1,4::Int] `shouldBe` [4,1,2,3,0]
  interleaveHalvesKeepExtremities [4,1,2,3,0::Int] `shouldBe` [0,3,1,2,4]
  interleaveHalvesKeepExtremities [0,3,1,2,4::Int] `shouldBe` [4,2,3,1,0]
  interleaveHalvesKeepExtremities [4,2,3,1,0::Int] `shouldBe` [0,1,2,3,4]

  -- with a list of length 6, 4 variations are /useful wrt topology/:
  interleaveHalvesKeepExtremities [0,1,2,3,4,5::Int] `shouldBe` [5,3,1,4,2,0]
  interleaveHalvesKeepExtremities [5,3,1,4,2,0::Int] `shouldBe` [0,4,3,2,1,5]
  interleaveHalvesKeepExtremities [0,4,3,2,1,5::Int] `shouldBe` [5,2,4,1,3,0]
  interleaveHalvesKeepExtremities [5,2,4,1,3,0::Int] `shouldBe` [0,1,2,3,4,5]

  --------------------------------------------------------------------------
  interleaveHalves' [0,1,2,3::Int] `shouldBe` [0,2,1,3]
  interleaveHalves' [0,2,1,3::Int] `shouldBe` [0,1,2,3]

  interleaveHalves' [0,1,2,3,4::Int] `shouldBe` [0,2,4,1,3]
  interleaveHalves' [0,2,4,1,3::Int] `shouldBe` [0,4,3,2,1]
  interleaveHalves' [0,4,3,2,1::Int] `shouldBe` [0,3,1,4,2]
  interleaveHalves' [0,3,1,4,2::Int] `shouldBe` [0,1,2,3,4]
