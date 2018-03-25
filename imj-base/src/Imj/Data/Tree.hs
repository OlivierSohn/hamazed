{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFoldable #-}

module Imj.Data.Tree
    ( LazyTree(..)
    , StrictTree(..)
    , StrictNTree(..)
    , StrictNTree2(..)
    , strictNTreeFromBranches
    , strictNTree2FromBranches
    , Filterable(..)
    ) where

import           Imj.Prelude hiding(filter)
import           Data.Set(Set)
import qualified Data.Set as Set (null, filter)
import qualified Data.List as List(filter)
import           Data.Foldable(foldl')

-- | Binary strict tree where values are stored in the leaves
data StrictTree a =
    StrictBranch !(StrictTree a) !(StrictTree a)
  | StrictLeaf !a
  | NoResult
  deriving (Foldable)
-- | Binary lazy tree where values are stored in the leaves
data LazyTree a =
    LazyBranch (LazyTree a) (LazyTree a)
  | LazyLeaf a
  | NoResult'
  deriving (Foldable)

-- | List-based K-way tree (with unspecified k) where values are stored inside the tree
data StrictNTree a =
    StrictNBranch !a ![StrictNTree a]
  | StrictNNothing
  deriving (Foldable)

{-# INLINE strictNTreeFromBranches #-}
strictNTreeFromBranches :: [StrictNTree a] -> StrictNTree a
strictNTreeFromBranches =
  go . List.filter hasValue
 where
  go [] = StrictNNothing
  go (StrictNBranch v b1:rest) = StrictNBranch v $ b1 ++ rest
  go (StrictNNothing:_) = error "logic"

{-# INLINE strictNTree2FromBranches #-}
strictNTree2FromBranches :: [StrictNTree2 a] -> StrictNTree2 a
strictNTree2FromBranches = StrictNBranch2 . List.filter hasValue

data StrictNTree2 a =
    StrictNBranch2 ![StrictNTree2 a]
  | StrictNLeaf2 !a
  deriving (Foldable)

class (Foldable a) => Filterable a where
  filter :: (b -> Bool) -> a b -> a b
  hasValue :: a b -> Bool

  countValues :: a b -> Int
  countValues = foldl' (\s _ -> succ s) 0

  toList :: a b -> [b]
  toList = foldl' (flip (:)) []

instance Filterable [] where
  filter = List.filter
  hasValue = not . null
  {-# INLINE filter #-}
  {-# INLINE hasValue #-}

instance Filterable Set where
  filter = Set.filter
  hasValue = not . Set.null
  {-# INLINE filter #-}
  {-# INLINE hasValue #-}

instance Filterable StrictTree where
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

  hasValue NoResult = False
  hasValue (StrictLeaf _) = True
  hasValue (StrictBranch _ _) = True
  {-# INLINE hasValue #-}


instance Filterable LazyTree where
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

  hasValue NoResult' = False
  hasValue (LazyLeaf _) = True
  hasValue (LazyBranch _ _) = True
  {-# INLINE hasValue #-}

instance Filterable StrictNTree where
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

  hasValue StrictNNothing = False
  hasValue (StrictNBranch _ _) = True
  {-# INLINE hasValue #-}


instance Filterable StrictNTree2 where
  filter p n@(StrictNLeaf2 l)
    | p l = n
    | otherwise = StrictNBranch2 []
  filter p (StrictNBranch2 l) = -- drop empty children
    StrictNBranch2 $
      List.filter hasValue $ map (filter p) l

  hasValue (StrictNLeaf2 _) = True
  hasValue (StrictNBranch2 l) = not $ null l
  {-# INLINE hasValue #-}
