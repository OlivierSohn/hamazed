{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Data.List
    ( -- * List utilities
      indexesBy
    , dedup
    , dedupAsc
    , replicateElements
    , intersperse'
    , splitEvery
    , mkGroups
    , mkEvenlySpreadGroups
    , range
    , commonPrefix
    , commonSuffix
    , maximumMaybe
    ) where

import           Prelude hiding(maximum)
import qualified Prelude as Unsafe(maximum)

import           Control.Exception(assert)
import           Data.List(foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


{-# INLINABLE indexesBy #-}
indexesBy :: Ord a
          => (v -> a)
          -- ^ Defines the ordering according to which the index is built.
          -> [v]
          -- ^ The 'List'
          -> [Int]
          -- ^ The list of indices (into the given 'List'), ascending
          -- according to the given function.
indexesBy f values =
  concat $ Map.elems $ fst $ foldl' (\(m,i) v -> (Map.insertWith (++) (f v) [i] m, i+1)) (Map.empty,0) values

-- | removes duplicates, and returns elements in ascending order.
{-# INLINABLE dedup #-}
dedup :: (Ord a) => [a] -> [a]
dedup = Set.toList . Set.fromList

-- | Same as 'dedup' except that the input is expected to be ascending.
{-# INLINABLE dedupAsc #-}
dedupAsc :: (Ord a) => [a] -> [a]
dedupAsc = Set.toList . Set.fromAscList

{-# INLINE maximumMaybe #-}
maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe = \case
  [] -> Nothing
  xs@(_:_) -> Just $ Unsafe.maximum xs


{-# INLINE replicateElements #-}
-- | Replicates each list element n times and concatenates the result.
replicateElements :: Int -> [a] -> [a]
replicateElements n = concatMap (replicate n)

-- | Divides a list in n lists of sizes s or s+1. Bigger lists are placed at the
-- beginning.
--
-- Elements order is maintained, i.e for every n>0 and input :
--
-- @ input == concat $ mkGroups n input @
{-# INLINABLE mkGroups #-}
mkGroups :: Int
         -- ^ number of groups, must be > 0
         -> [a]
         -> [[a]]
mkGroups n elts
  | n <= 0 = error $ "negative group count " ++ show n
  | otherwise = reverse $ assert (null remainingElts) groups
  where
    l = length elts
    (minGroupSize,remain) = quotRem l n
    sizes = replicate remain (succ minGroupSize) ++ replicate (n-remain) minGroupSize
    (remainingElts, groups) =
      foldl'
        (\(rElts,res) sz ->
          let (a,rest) = splitAt sz rElts
          in (rest,a:res))
        (elts, [])
        sizes

-- | Divides a list in n lists of sizes s or s+1. Bigger lists are evenly spread.
--
-- Elements order is maintained, i.e for every n>0 and input :
--
-- @ input == concat $ mkGroups n input @
mkEvenlySpreadGroups :: Int
                     -- ^ number of groups, must be > 0
                     -> [a]
                     -> [[a]]
mkEvenlySpreadGroups n elts
  | n <= 0 = error $ "negative group count " ++ show n
  | otherwise = reverse $ go 0 (n-1) elts []
 where
  groupSz :: Float
  groupSz = fromIntegral (length elts) / fromIntegral n

  go _ 0 remaining groups = remaining:groups
  go consumed i remaining groups =
    go (consumed + thisGroupSz) (i-1) next $ group:groups
   where
    thisGroupSz = round (groupSz * (fromIntegral $ n-i)) - consumed
    (group,next) = splitAt thisGroupSz remaining

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs =
    as : splitEvery n bs
  where
    (as,bs) = splitAt n xs


{-# INLINABLE range #-}
{- | Builds a range with no constraint on the order of bounds:

@
range 3 5 == [3,4,5]
range 5 3 == [5,4,3]
@
-}
range :: Enum a => Ord a
      => a -- ^ First inclusive bound
      -> a -- ^ Second inclusive bound
      -> [a]
range n m =
  if m < n
    then
      [n,(pred n)..m]
    else
      [n..m]

{-# INLINABLE commonPrefix #-}
commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix (x:xs) (y:ys)
    | x == y    = x : commonPrefix xs ys
commonPrefix _ _ = []

{-# INLINABLE commonSuffix #-}
commonSuffix :: (Eq a) => [a] -> [a] -> [a]
commonSuffix s s' = reverse $ commonPrefix (reverse s) (reverse s')


-- from https://hackage.haskell.org/package/text-1.2.3.0
intersperse' :: a -> [a] -> [a]
intersperse' _   []     = []
intersperse' sep (x:xs) = x : go xs
  where
    go []     = []
    go (y:ys) = sep : y: go ys
{-# INLINE intersperse' #-}
