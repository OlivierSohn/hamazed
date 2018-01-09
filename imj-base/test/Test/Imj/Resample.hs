module Test.Imj.Resample
         ( testResample
         ) where

import           Imj.Prelude

import           Imj.Geo.Discrete

-- | returns True on success, else errors
testResample :: IO Bool
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
  return True
