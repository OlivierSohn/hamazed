{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Sums
    ( mkSums
    , mkSumsArray
    , mkSumsArray'
    , mkSumsStrict
    , mkSumsLazy
    ) where

import           Imj.Prelude

import           Data.List(reverse)
import           Data.Set(Set)
import qualified Data.Set as Set(toList, fromList)
import qualified Data.Vector.Storable as Storable(fromList, length, unsafeIndex)

import           Imj.Tree

-- | Assumes that the input is an /ascending/ list of /unique/ /positive/ numbers.
-- Computes the list of lists whose elements are in the input list and whose sum
-- equate the target number.
mkSums :: Set Int -> Int -> Set (Set Int)
mkSums allNumbers total =
  Set.fromList $ map Set.fromList $
    go (Set.toList allNumbers) total []
 where
  go [] !target curNums
    | target == 0 = [curNums]
    | otherwise = []
  go (n:rest) !target curNums
    | target == 0 = [curNums]
    | target < n = []
    | otherwise = concat
      [ go rest (target - n) (n:curNums) -- in this branch, we take the number
      , go rest target curNums -- in this branch, we drop the number
      ]

-- A version using a storable vector.
mkSumsArray :: Set Int -> Int -> Set (Set Int)
mkSumsArray a b =
  Set.fromList $ map Set.fromList $ mkSumsArray' a b

-- A version using a storable vector, and using lists as output.
mkSumsArray' :: Set Int -> Int -> [[Int]]
mkSumsArray' allNumbers total =
  go array (Storable.length array) total []
 where
  -- we reverse the numbers because we will iterate on the array from the last to the first element.
  array = Storable.fromList $ reverse $ Set.toList allNumbers
  go _ 0 !target curNums
    | target == 0 = [curNums]
    | otherwise = []
  go a !i !target curNums
    | target == 0 = [curNums]
    | target < n = []
    | otherwise = concat
      [ go a index (target - n) (n:curNums) -- in this branch, we take the number
      , go a index target curNums -- in this branch, we drop the number
      ]
    where
      !index = pred i
      n = Storable.unsafeIndex a index

-- A version using a storable vector, and a strict tree as output.
mkSumsStrict :: Set Int -> Int -> StrictTree [Int]
mkSumsStrict allNumbers total =
  go array (Storable.length array) total []
 where
  -- we reverse the numbers because we will iterate on the array from the last to the first element.
  array = Storable.fromList $ reverse $ Set.toList allNumbers
  go _ 0 !target curNums
    | target == 0 = StrictLeaf curNums
    | otherwise = NoResult
  go a i !target curNums
    | target == 0 = StrictLeaf curNums
    | target < n = NoResult
    | otherwise = case left of
        NoResult -> right
        _ -> case right of
          NoResult -> left
          _ -> StrictBranch left right
    where
      !index = pred i
      !n = Storable.unsafeIndex a index
      left = go a index (target - n) (n:curNums) -- in this branch, we take the number
      right = go a index target curNums -- in this branch, we drop the number



-- A version using a storable vector, and a lazy tree as output.
mkSumsLazy :: Set Int -> Int -> LazyTree [Int]
mkSumsLazy allNumbers total =
  go array (Storable.length array) total []
 where
  -- we reverse the numbers because we will iterate on the array from the last to the first element.
  array = Storable.fromList $ reverse $ Set.toList allNumbers
  go _ 0 target curNums
    | target == 0 = LazyLeaf curNums
    | otherwise = NoResult'
  go a i target curNums
    | target == 0 = LazyLeaf curNums
    | target < n = NoResult'
    | otherwise = case left of
        NoResult' -> right
        _ -> case right of
          NoResult' -> left
          _ -> LazyBranch left right
    where
      index = pred i
      n = Storable.unsafeIndex a index
      left = go a index (target - n) (n:curNums) -- in this branch, we take the number
      right = go a index target curNums -- in this branch, we drop the number
