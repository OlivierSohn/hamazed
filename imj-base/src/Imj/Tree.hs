{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Tree
    ( LazyTree(..)
    , StrictTree(..)
    , StrictNTree(..)
    , strictNTreeFromBranches
    , Filterable(..)
    ) where

import Imj.Prelude hiding(filter)
import qualified Data.List as List(filter, length, foldl')

-- | Binary strict tree where values are stored in the leaves
data StrictTree a =
    StrictBranch !(StrictTree a) !(StrictTree a)
  | StrictLeaf !a
  | NoResult
-- | Binary lazy tree where values are stored in the leaves
data LazyTree a =
    LazyBranch (LazyTree a) (LazyTree a)
  | LazyLeaf a
  | NoResult'

-- | List-based K-way tree (with unspecified k) where values are stored inside the tree
data StrictNTree a =
    StrictNBranch !a ![StrictNTree a]
  | StrictNNothing

{-# INLINE strictNTreeFromBranches #-}
strictNTreeFromBranches :: [StrictNTree a] -> StrictNTree a
strictNTreeFromBranches = go . List.filter hasValue
 where
  hasValue = \case
    StrictNNothing -> False
    StrictNBranch _ _ -> True
  go [] = StrictNNothing
  go (StrictNBranch v b1:rest) = StrictNBranch v $ b1 ++ rest
  go (StrictNNothing:_) = error "logic"

class Filterable a where
  filter :: (b -> Bool) -> a b -> a b
  countValues :: a b -> Int
  filter' :: (b -> Bool) -> a b -> [b]
  toList :: a b -> [b]

instance Filterable [] where
  filter' = List.filter
  filter = List.filter
  countValues = List.length
  toList = id

instance Filterable StrictTree where
  filter' _ NoResult = []
  filter' p (StrictLeaf l)
    | p l = [l]
    | otherwise = []
  filter' p (StrictBranch left right) =
    filter' p left ++ filter' p right

  filter _ NoResult = NoResult
  filter p n@(StrictLeaf l)
    | p l = n
    | otherwise = NoResult
  filter p (StrictBranch left right) =
    case leftFiltered of
      NoResult -> rightFiltered
      _ -> case rightFiltered of
        NoResult -> leftFiltered
        _ -> StrictBranch leftFiltered rightFiltered
    where
      rightFiltered = filter p right
      leftFiltered  = filter p left

  countValues NoResult = 0
  countValues (StrictLeaf _) = 1
  countValues (StrictBranch l r) =
    countValues l + countValues r

  toList NoResult = []
  toList (StrictLeaf l) = [l]
  toList (StrictBranch left right) =
    toList left ++ toList right
instance Filterable LazyTree where
  filter' _ NoResult' = []
  filter' p (LazyLeaf l)
    | p l = [l]
    | otherwise = []
  filter' p (LazyBranch left right) =
    filter' p left ++ filter' p right

  filter _ NoResult' = NoResult'
  filter p n@(LazyLeaf l)
    | p l = n
    | otherwise = NoResult'
  filter p (LazyBranch left right) =
    case leftFiltered of
      NoResult' -> rightFiltered
      _ -> case rightFiltered of
        NoResult' -> leftFiltered
        _ -> LazyBranch leftFiltered rightFiltered
    where
      rightFiltered = filter p right
      leftFiltered  = filter p left

  countValues NoResult' = 0
  countValues (LazyLeaf _) = 1
  countValues (LazyBranch l r) =
    countValues l + countValues r

  toList NoResult' = []
  toList (LazyLeaf l) = [l]
  toList (LazyBranch left right) =
    toList left ++ toList right


instance Filterable StrictNTree where
  filter' _ StrictNNothing = []
  filter' p (StrictNBranch a l) =
    a : concatMap (filter' p) l

  filter _ StrictNNothing = StrictNNothing
  filter p (StrictNBranch a l) = -- drop empty children
    strictNTreeFromBranches $
      if p a
        then
          StrictNBranch a [] : filteredChildren
        else
          filteredChildren
   where
    filteredChildren = map (filter p) l

  countValues StrictNNothing = 0
  countValues (StrictNBranch _ l) =
    List.foldl' (\n b -> n + countValues b) 1 l

  toList StrictNNothing = []
  toList (StrictNBranch a l) =
    a : concatMap toList l
