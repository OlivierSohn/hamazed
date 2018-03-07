{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Sums -- TODO allow non unique elements
    ( mkSums
    , mkSumsN
    , mkSumsArray
    , mkSumsArray'
    , mkSumsStrict
    , mkSumsStrict2
    , mkSumsStrictN
    , mkSumsStrictN2
    , mkSumsLazy
    -- for white box testing
    , asOccurences
    , ValueOccurences(..)
    ) where

import           Imj.Prelude

import           Data.List(reverse, length, break, null)
import qualified Data.List as List(filter)
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
  go (Storable.length array) total []
 where
  -- we reverse the numbers because we will iterate on the array from the last to the first element.
  array = Storable.fromList $ reverse $ Set.toList allNumbers
  go 0 !target curNums
    | target == 0 = [curNums]
    | otherwise = []
  go !i !target curNums
    | target == 0 = [curNums]
    | target < n = []
    | otherwise = concat
      [ go index (target - n) (n:curNums) -- in this branch, we take the number
      , go index target curNums -- in this branch, we drop the number
      ]
    where
      !index = pred i
      n = Storable.unsafeIndex array index

-- A version using a storable vector, and a strict tree as output.
mkSumsStrict :: Set Int -> Int -> StrictTree [Int]
mkSumsStrict allNumbers total =
  go (Storable.length array) total []
 where
  -- we reverse the numbers because we will iterate on the array from the last to the first element.
  array = Storable.fromList $ reverse $ Set.toList allNumbers
  go 0 !target curNums
    | target == 0 = StrictLeaf curNums
    | otherwise = NoResult
  go i !target curNums
    | target == 0 = StrictLeaf curNums
    | target < n = NoResult
    | otherwise = case left of
        NoResult -> right
        _ -> case right of
          NoResult -> left
          _ -> StrictBranch left right
    where
      !index = pred i
      !n = Storable.unsafeIndex array index
      left = go index (target - n) (n:curNums) -- in this branch, we take the number
      right = go index target curNums -- in this branch, we drop the number

-- A version using a strict tree as output
mkSumsStrict2 :: Set Int -> Int -> StrictTree [Int]
mkSumsStrict2 allNumbers total =
  go (Set.toList allNumbers) total []
 where
  -- we reverse the numbers because we will iterate on the array from the last to the first element.
  go [] !target curNums
    | target == 0 = StrictLeaf curNums
    | otherwise = NoResult
  go (n:rest) !target curNums
    | target == 0 = StrictLeaf curNums
    | target < n = NoResult
    | otherwise = case left of
        NoResult -> right
        _ -> case right of
          NoResult -> left
          _ -> StrictBranch left right
    where
      left = go rest (target - n) (n:curNums) -- in this branch, we take the number
      right = go rest target curNums -- in this branch, we drop the number



-- A version using a storable vector, and a lazy tree as output.
mkSumsLazy :: Set Int -> Int -> LazyTree [Int]
mkSumsLazy allNumbers total =
  go (Storable.length array) total []
 where
  -- we reverse the numbers because we will iterate on the array from the last to the first element.
  array = Storable.fromList $ reverse $ Set.toList allNumbers
  go 0 target curNums
    | target == 0 = LazyLeaf curNums
    | otherwise = NoResult'
  go i target curNums
    | target == 0 = LazyLeaf curNums
    | target < n = NoResult'
    | otherwise = case left of
        NoResult' -> right
        _ -> case right of
          NoResult' -> left
          _ -> LazyBranch left right
    where
      index = pred i
      n = Storable.unsafeIndex array index
      left = go index (target - n) (n:curNums) -- in this branch, we take the number
      right = go index target curNums -- in this branch, we drop the number

data ValueOccurences = ValueOccurences {
    _countOccurences :: {-# UNPACK #-} !Int
  , _value :: {-# UNPACK #-} !Int
} deriving(Generic, Show, Eq)

-- | Expects an ascending input list. Returns a list in descending order.
asOccurences :: [Int] -> [ValueOccurences]
asOccurences l = go l []
 where
  go [] occurences = occurences
  go (e:rest) occurences =
    let (sameAfter, different) = break (e /=) rest
        thisOccurences =  ValueOccurences (succ $ length sameAfter) e
    in go different $ thisOccurences : occurences

-- | Same as 'mkSumsStrict' except the ascending input can contain duplicate elements,
-- and the output is a 'StrictNTree' of ascending lists.
--
-- It is also 4x slower than 'mkSumsStrict' / 'mkSumsStrict2' which do not offer
-- the possibility to have duplicate elements
mkSumsStrictN :: [Int] -> Int -> StrictNTree [Int]
mkSumsStrictN allNumbers total =
  go (map combinations $ reverse $ asOccurences allNumbers) total []
 where
  go [] !target curNums
    | target == 0 = StrictNBranch (concat curNums) []
    | otherwise = StrictNNothing
  go (occurencesCombinations:rest) !target curNums =
      strictNTreeFromBranches $ map oneBranch occurencesCombinations
    where
      oneBranch (Combination values value) =
        go rest (target - value) $ values : curNums

mkSumsStrictN2 :: [Int] -> Int -> StrictNTree2 [Int]
mkSumsStrictN2 allNumbers total =
  go (map combinations $ reverse $ asOccurences allNumbers) total []
 where
  go [] !target curNums
    | target == 0 = StrictNLeaf2 $ concat curNums
    | otherwise = StrictNBranch2 []
  go (occurencesCombinations:rest) !target curNums =
      strictNTree2FromBranches $ map oneBranch occurencesCombinations
    where
      oneBranch (Combination values value) =
        go rest (target - value) $ values : curNums

-- | Same as 'mkSumsStrictN' except the output is a list of lists.
mkSumsN :: [Int] -> Int -> [[Int]]
mkSumsN allNumbers total =
  go (map combinations $ reverse $ asOccurences allNumbers) total []
 where
  go [] !target curNums
    | target == 0 = [concat curNums]
    | otherwise = []
  go (occurencesCombinations:rest) !target curNums =
      -- filter before concat leads to better performances
      concat $ List.filter (not . null) $ map oneBranch occurencesCombinations
    where
      oneBranch (Combination values value) =
        go rest (target - value) $ values : curNums

data Combination = Combination ![Int] {-# UNPACK #-} !Int

combinations :: ValueOccurences -> [Combination]
combinations (ValueOccurences n v) =
  map (\i -> Combination (replicate i v) $ i * v) [0..n]
