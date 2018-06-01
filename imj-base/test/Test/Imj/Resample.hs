module Test.Imj.Resample
         ( testResample
         ) where

import           Imj.Prelude

import           Imj.Geo.Discrete
import           Imj.Data.AlmostFloat

testResample :: IO ()
testResample = do
  let r = resampleWithExtremities [0::Int ..] 4 4
      r' = resampleWithExtremities [0..3::Int] 4 4
  unless (r == [0..3]) $ error $ "wrong single resample" ++ show r
  unless (r' == [0..3]) $ error $ "wrong single resample'" ++ show r'
  let d = resampleWithExtremities [0::Int ..] 4 8
  unless (d == [0,0,1,1,2,2,3,3]) $ error $ "wrong double resample" ++ show d
  let m = resampleWithExtremities [0::Int ..] 5 1
      m' = resampleWithExtremities [0..4::Int] 5 1
  unless (m == [2]) $ error $ "wrong middle resample" ++ show m
  unless (m' == [2]) $ error $ "wrong middle resample'" ++ show m'
  let t = resampleWithExtremities [0::Int ..] 5 2
      t' = resampleWithExtremities [0..4::Int] 5 2
  unless (t == [0,4]) $ error $ "wrong 2 resample" ++ show t
  unless (t' == [0,4]) $ error $ "wrong 2 resample'" ++ show t'
  let three = resampleWithExtremities [0::Int ..] 5 3
      three' = resampleWithExtremities [0..4::Int] 5 3
  unless (three == [0,2,4]) $ error $ "wrong 3 resample" ++ show three
  unless (three' == [0,2,4]) $ error $ "wrong 3 resample'" ++ show three'
  let four = resampleWithExtremities [0::Int ..] 5 4
      four' = resampleWithExtremities [0..4::Int] 5 4
  unless (four == [0,1,3,4]) $ error $ "wrong 4 resample" ++ show four
  unless (four' == [0,1,3,4]) $ error $ "wrong 4 resample'" ++ show four'
  let six = resampleWithExtremities [0::Int ..] 5 6
      six' = resampleWithExtremities [0..4::Int] 5 6
  unless (six == [0,1,2,2,3,4]) $ error $ "wrong 6 resample" ++ show six
  unless (six' == [0,1,2,2,3,4]) $ error $ "wrong 6 resample'" ++ show six'

  resampleMinMaxLinear [] 0 0 `shouldBe` map toAlmost []
  resampleMinMaxLinear [] 2 2 `shouldBe` map toAlmost []
  resampleMinMaxLinear [] 0 2 `shouldBe` map toAlmost []
  resampleMinMaxLinear [] 2 0 `shouldBe` map toAlmost []
  resampleMinMaxLinear [1,2,3,4] 4 9 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 2 1,MinMax 3 3 1,MinMax 4 4 1]
  resampleMinMaxLinear [1,2,3,4] 4 5 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 2 1,MinMax 3 3 1,MinMax 4 4 1]
  resampleMinMaxLinear [1,2,3,4] 4 4 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 2 1,MinMax 3 3 1,MinMax 4 4 1]
  resampleMinMaxLinear [1,2,3,4] 4 3 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 2 1,MinMax 3 4 2]
  resampleMinMaxLinear [1,2,3,4] 4 2 `shouldBe` map toAlmost [MinMax 1 2 2,MinMax 3 4 2]
  resampleMinMaxLinear [1,2,3,4] 4 1 `shouldBe` map toAlmost [MinMax 1 4 4]
  resampleMinMaxLinear [1,2,3] 3 3 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 2 1,MinMax 3 3 1]
  resampleMinMaxLinear [1,2,3] 3 2 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 3 2]
  resampleMinMaxLinear [1,2,3] 3 1 `shouldBe` map toAlmost [MinMax 1 3 3]
  resampleMinMaxLinear [1,2,3] 3 0 `shouldBe` map toAlmost []

  resampleMinMaxLogarithmic [] 0 0 `shouldBe` map toAlmost []
  resampleMinMaxLogarithmic [] 2 2 `shouldBe` map toAlmost []
  resampleMinMaxLogarithmic [] 0 2 `shouldBe` map toAlmost []
  resampleMinMaxLogarithmic [] 2 0 `shouldBe` map toAlmost []
  resampleMinMaxLogarithmic [1,2,3,4] 4 1 `shouldBe` map toAlmost [MinMax 1 4 4]
  resampleMinMaxLogarithmic [1,2,3,4] 4 2 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 4 3]
  resampleMinMaxLogarithmic [1,2,3,4] 4 3 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 2 1,MinMax 3 4 2]
  resampleMinMaxLogarithmic [1,2,3,4] 4 4 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 2 1,MinMax 3 3 1,MinMax 4 4 1]
  resampleMinMaxLogarithmic [1,2,3,4] 4 5 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 2 1,MinMax 3 3 1,MinMax 4 4 1]
  resampleMinMaxLogarithmic [1,2,3] 3 0 `shouldBe` map toAlmost []
  resampleMinMaxLogarithmic [1,2,3] 3 1 `shouldBe` map toAlmost [MinMax 1 3 3]
  resampleMinMaxLogarithmic [1,2,3] 3 2 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 3 2]
  resampleMinMaxLogarithmic [1,2,3] 3 3 `shouldBe` map toAlmost [MinMax 1 1 1,MinMax 2 2 1,MinMax 3 3 1]

  let res2 = resampleMinMaxLogarithmic (map (almost . fromIntegral) [0::Int ..]) 15 4
  res2 `shouldBe` map toAlmost [MinMax 0 0 1,MinMax 1 2 2,MinMax 3 6 4,MinMax 7 14 8]
  map ((+ 1) . mmSpan) res2 `shouldBe` [1,2,4,8]

  let res10 = resampleMinMaxLogarithmic (map (almost . fromIntegral) [0::Int ..]) 1111 4
  res10 `shouldBe` map toAlmost [MinMax 0 0 1,MinMax 1 10 10,MinMax 11 110 100,MinMax 111 1110 1000]
  map ((+ 1) . mmSpan) res10 `shouldBe` [1,10,100,1000]


toAlmost :: MinMax Float -> MinMax AlmostFloat
toAlmost (MinMax a b n) = MinMax (almost a) (almost b) n

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
