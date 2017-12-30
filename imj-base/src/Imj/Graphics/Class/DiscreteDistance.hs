{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Graphics.Class.DiscreteDistance
        ( DiscreteDistance(..)
        , Successive(..)
        ) where

import           Imj.Prelude

import           Data.List( length )

import           Imj.Geo.Discrete
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Text.ColorString.Interpolation
import           Imj.Util

-- | Wrapper on a list, to represents successive waypoints.
newtype Successive a = Successive [a] deriving(Show)

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


-- | Using bresenham 2d line algorithm.
instance DiscreteDistance (Coords Pos) where
  distance = bresenhamLength

-- | Using bresenham 3D algorithm in RGB space. Only valid between 2 'rgb' or 2 'gray'.
instance DiscreteDistance (Color8 a) where
  -- | The two input 'Color8' are supposed to be both 'rgb' or both 'gray'.
  distance = bresenhamColor8Length


-- TODO use bresenham 6 to interpolate foreground and background at the same time:
-- https://nenadsprojects.wordpress.com/2014/08/08/multi-dimensional-bresenham-line-in-c/
-- | First interpolate background color, then foreground color
instance DiscreteDistance LayeredColor where
  distance (LayeredColor bg fg) (LayeredColor bg' fg') =
    succ $ pred (distance bg bg') + pred (distance fg fg')

-- TODO maybe it would be faster to have a representation with Array (Char, LayeredColor)
--  (ie the result of simplify)
-- | First interpolating characters, then color.
instance DiscreteDistance ColorString where
  distance c1 c2 =
    let colorDist (_, color) (_, color') = distance color color'
        n1 = countChars c1
        n2 = countChars c2
        s1 = simplify c1
        s2 = simplify c2

        (c1', remaining) = interpolateChars s1 s2 countTextChanges
        s1' = assert (remaining == 0) c1'
        l = zipWith colorDist s1' s2 -- since color interpolation happends AFTER char changes,
                                     -- we compare colors with result of char interpolation
        colorDistance =
          if null l
            then
              1
            else
              maximum l

        toString = map fst
        str1 = toString s1
        str2 = toString s2
        lPref = length $ commonPrefix str1 str2
        lSuff = length $ commonSuffix (drop lPref str1) (drop lPref str2)
        countTextChanges = max n1 n2 - (lPref + lSuff)
    in colorDistance + countTextChanges
