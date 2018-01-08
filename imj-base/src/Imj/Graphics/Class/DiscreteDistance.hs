{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.DiscreteDistance
        ( DiscreteDistance(..)
        , Successive(..)
        , concatSuccessive
        ) where

import           Imj.Prelude

import           Data.List( length )

-- | Wrapper on a list, to represents successive waypoints.
newtype Successive a = Successive [a] deriving(Show)

instance Functor Successive where
  fmap f (Successive l) = Successive $ fmap f l

concatSuccessive :: Successive x -> Successive x -> Successive x
concatSuccessive (Successive a) (Successive b) =
  Successive $ a++b

{- | Instances should satisfy:

\( \forall (\, from, to)\, \in v, \)

* 'distance' @from to@ >= 0
* 'distance' @from to@ can be different from 'distance' @to from@,
to provide different forward and backward interpolations (or morphings).
-}
class DiscreteDistance v where
  -- | Distance between two 'DiscreteDistance's.
  distance :: v -- ^ first value
           -> v -- ^ last value
           -> Int -- ^ the number of steps (including first and last) to go from first to last

  -- | Distance between n successive 'DiscreteDistance's.
  distanceSuccessive :: Successive v
                     -> Int
  distanceSuccessive (Successive []) =
    error "empty successive"
  distanceSuccessive (Successive l@(_:_)) =
    succ $ sum $ zipWith (\a b -> pred $ distance a b) l $ tail l

-- | Naïve interpolation.
instance DiscreteDistance Int where
  distance i i' =
    1 + abs (i-i')

-- | Interpolation between 2 lists, occuring in parallel between same-index elements.
--   Prerequisite : lists have the same lengths.
--
--  For an interpolation that occurs sequentially between same-index elements,
--   use SequentiallyInterpolatedList.
instance (DiscreteDistance a)
      => DiscreteDistance ([] a) where
  distance [] _ = 1
  distance _ [] = 1
  distance l l' =
    maximum $ zipWith distance l $ assert (length l == length l') l'
