{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Graphics.Class.DiscreteInterpolation
        ( DiscreteInterpolation(..)
          -- * Reexport
        , module Imj.Graphics.Class.DiscreteDistance
        ) where

import           Imj.Prelude

import           Data.List(length)

import           Imj.Graphics.Class.DiscreteDistance
import           Imj.Util
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types

{- | Instances should statisfy the following constraints:

* An interpolation between A and B starts at A and ends at B:

\( \forall (\, from, to)\, \in v, \)

@
    d = distance from to
    interpolate from to 0 == from
    interpolate from to d == to
@

* The interpolation path is composed of strictly distinct points:

@
    length $ nubOrd $ map (interpolate from to) [0..pred d] == d
@

* Given any points A,B belonging the path generated by an interpolation,
  the interpolation beween A and B will be the points of the path between A and B:

\( \forall med \in [\,0,d]\,, \forall low \in [\,0,med]\,, \forall high \in [\,med,d]\,, \)

@
    distance from med + distance med to == 1 + distance from to
    medVal = interpolate from to med
    interpolate from to low  == interpolate from medVal low
    interpolate from to high == interpolate medVal to $ high-med
@
-}
class (DiscreteDistance v) => DiscreteInterpolation v where
  -- | When the current step is outside bounds, the interplated value is one
  -- of the first and last values.
  interpolate :: v -- ^ first value
              -> v -- ^ last value
              -> Int -- ^ the current step
              -> v -- ^ the interpolated value

  -- | When the current step is outside bounds, we use 'zigzag' to interpolate
  -- back and forth.
  interpolateCyclic :: v -- ^ first value
                    -> v -- ^ last value
                    -> Int -- ^ the current step
                    -> v -- ^ the interpolated value
  interpolateCyclic a b =
      interpolate a b . zigzag 0 lf
    where
      lf = pred $ distance a b

  interpolateSuccessive :: Successive v
                        -> Int
                        -> v
  interpolateSuccessive (Successive []) _ = error "empty successive"
  interpolateSuccessive (Successive [a]) _ = a
  interpolateSuccessive (Successive (a:rest@(b:_))) i
    | i <= 0      = a
    | i >= lf = interpolateSuccessive (Successive rest) $ i-lf
    | otherwise = interpolate a b i
    where lf = pred $ distance a b

-- | Naïve interpolation.
instance DiscreteInterpolation Int where
  interpolate i i' progress =
    i + signum (i'-i) * clamp progress 0 (abs (i-i'))


-- | Interpolate in parallel between 2 lists : each pair of same-index elements
-- is interpolated at the same time.
instance (DiscreteInterpolation a)
      => DiscreteInterpolation ([] a) where
  interpolate l l' progress =
    zipWith (\e e' -> interpolate e e' progress) l $ assert (length l == length l') l'


-- | Using bresenham 2d line algorithm.
instance DiscreteInterpolation (Coords Pos) where
  interpolate c c' i
    | c == c' = c
    | otherwise =
        let lastFrame = pred $ fromIntegral $ bresenhamLength c c'
        in bresenham (mkSegment c c') !! clamp i 0 lastFrame

-- | Using bresenham 3D algorithm in RGB space.
instance DiscreteInterpolation (Color8 a) where
  interpolate c c' i
    | i >= lastFrame = c'
    | otherwise = bresenhamColor8 c c' !! clamp i 0 lastFrame
    where
      !lastFrame = pred $ bresenhamColor8Length c c'

-- | First interpolate background color, then foreground color
instance DiscreteInterpolation LayeredColor where
  interpolate (LayeredColor bg fg) (LayeredColor bg' fg') i
    | i < lastBgFrame = LayeredColor (interpolate bg bg' i) fg
    | otherwise       = LayeredColor bg' $ interpolate fg fg' $ i - lastBgFrame
    where
      lastBgFrame = pred $ distance bg bg'
