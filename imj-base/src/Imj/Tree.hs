{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Tree
    ( LazyTree(..)
    , StrictTree(..)
    , Filterable(..)
    ) where

import Imj.Prelude hiding(filter)

data StrictTree a =
    StrictBranch !(StrictTree a) !(StrictTree a)
  | StrictLeaf a
  | NoResult
data LazyTree a =
    LazyBranch (LazyTree a) (LazyTree a)
  | LazyLeaf a
  | NoResult'

class Filterable a where
  filter' :: (b -> Bool) -> a b -> [b]
  filter :: (b -> Bool) -> a b -> a b
  toList :: a b -> [b]

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

  toList NoResult' = []
  toList (LazyLeaf l) = [l]
  toList (LazyBranch left right) =
    toList left ++ toList right
