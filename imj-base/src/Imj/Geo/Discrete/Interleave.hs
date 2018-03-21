{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Discrete.Interleave
    ( mkInterleaveData
    , countUsefullInterleavedVariations
    , interleaveHalves
    , interleaveHalvesKeepExtremities
    , interleaveHalves'
    , interleaveHalves''
    , interleaveIdx
    ) where

import           Imj.Prelude

import           Data.List(length, (!!))
import           Imj.Util

{- Definition: a list interleaved variation is /useful wrt topology/ if itself,
a rotation of itself or the inverse of that has not been
encountered before in the cycle formed by applying 'interleaveHalves' repeatedly.

Note that once we encounter an equivalent representation,
all subsequent representations are equivalent to an already encoutered one.

This function gives an approximation, for values > 20 and exact values for values below.

The reason I didn't take the time to hardcode values > 20 is because for Hamazed game,
I don't need them. We could use TH to hardcode more values (cf 'testInterleaveHalves' for algo).
-}
{-# INLINE countUsefullInterleavedVariations #-}
countUsefullInterleavedVariations :: Int -> Int
countUsefullInterleavedVariations 0 = 1
countUsefullInterleavedVariations 1 = 1
countUsefullInterleavedVariations 2 = 1
countUsefullInterleavedVariations 3 = 1
countUsefullInterleavedVariations 4 = 2
countUsefullInterleavedVariations 5 = 2
countUsefullInterleavedVariations 6 = 3
countUsefullInterleavedVariations 7 = 3
countUsefullInterleavedVariations 8 = 3
countUsefullInterleavedVariations 9 = 3
countUsefullInterleavedVariations 10 = 5
countUsefullInterleavedVariations 11 = 5
countUsefullInterleavedVariations 12 = 6
countUsefullInterleavedVariations 13 = 6
countUsefullInterleavedVariations 14 = 4
countUsefullInterleavedVariations 15 = 4
countUsefullInterleavedVariations 18 = 9
countUsefullInterleavedVariations 19 = 9
countUsefullInterleavedVariations 20 = 6
countUsefullInterleavedVariations l = logBase2 l

-- | Interleave the first half with the second half.
interleaveHalves :: [a] -> [a]
interleaveHalves l =
  uncurry (++) $ go [] [] l
 where
  go l1 l2 []         = (   l1,l2)
  go l1 l2 [x1]       = (x1:l1,l2)
  go l1 l2 (x1:x2:xs) = go (x1:l1) (x2:l2) xs


-- | Interleave the first half with the second half, and keep extremal elements (first and last)
-- at the extremities (maybe swapped).
interleaveHalvesKeepExtremities :: [a] -> [a]
interleaveHalvesKeepExtremities l =
  uncurry (++) $ go [] [] l
 where
  go l1 l2 []         = (l1,l2)
  go l1 l2 [x1]       = (x1:l1,l2)
  go l1 l2 (x1:x2:xs) = go (x2:l1) (x1:l2) xs

-- | in a more imperative-style than interleaveHalves, with the same outcome
interleaveHalves'' :: [a] -> [a]
interleaveHalves'' l =
  map (\i -> l !! i) modIndices
 where
  len = length l
  indices = [0..len-1]
  inter = mkInterleaveData len
  modIndices = map
    (interleaveIdx inter)
    indices

data InterleaveData = InterleaveData {
    _firstOddIdx, _halfLength :: {-# UNPACK #-} !Int
}

mkInterleaveData :: Int -> InterleaveData
mkInterleaveData len =
  InterleaveData firstOddIdx halfLength
 where
  (halfLength, r)  = quotRem len 2
  firstOddIdx = halfLength + bool 1 0 (r == 0)

{-# INLINE interleaveIdx #-}
interleaveIdx :: InterleaveData -> Int -> Int
interleaveIdx (InterleaveData firstOddIdx halfLength) i =
  let i' = firstOddIdx-i
  in if i' > 0
      then
        2 * (i' - 1)
      else
        2 * (halfLength + i') - 1

-- | Keeps the first element in place.
interleaveHalves' :: [a] -> [a]
interleaveHalves' l =
  map (\i -> l !! i) modIndices
 where
  len = length l
  indices = [0..len-1]
  firstOddIdx = quot (len+1) 2
  modIndices =
    map
      (\i -> if i < firstOddIdx then 2*i else 1 + 2 *(i-firstOddIdx))
      indices
