{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Graphics.Class.DiscreteDistance
        ( DiscreteDistance(..)
        , Successive(..)
        , concatSuccessive
        ) where

import           Imj.Prelude
import qualified Prelude as Unsafe(last, maximum)

import           Data.List( length )
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Class.Positionable

-- | Wrapper on a list, to represents successive waypoints.
newtype Successive a = Successive [a]
  deriving(Show, Generic, PrettyVal)
instance Functor Successive where
  fmap f (Successive l) = Successive $ fmap f l
instance (HasReferencePosition a) => HasReferencePosition (Successive a) where
  getPosition (Successive []) = zeroCoords -- should we have a Maybe to handle this?
  getPosition (Successive l) = getPosition $ Unsafe.last l
  {-# INLINE getPosition #-}
instance (GeoTransform a) => GeoTransform (Successive a) where
  transform f = fmap (transform f)
  {-# INLINE transform #-}

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
    0
  distanceSuccessive (Successive l@(_:rest)) =
    1 + sum (zipWith (\a b -> distance a b - 1) l rest)

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
    Unsafe.maximum $ zipWith distance l $ assert (length l == length l') l'

-- | Using bresenham 2d line algorithm.
instance DiscreteDistance (Coords Pos) where
  distance a b = fromIntegral $ bresenhamLength a b

-- | Using bresenham 3D algorithm in RGB space.
instance DiscreteDistance (Color8 a) where
  distance = bresenhamColor8Length

-- TODO use bresenham 6 to interpolate foreground and background at the same time:
-- https://nenadsprojects.wordpress.com/2014/08/08/multi-dimensional-bresenham-line-in-c/
-- | First interpolate background color, then foreground color
instance DiscreteDistance LayeredColor where
  distance (LayeredColor bg fg) (LayeredColor bg' fg') =
    distance bg bg' + distance fg fg' - 1
