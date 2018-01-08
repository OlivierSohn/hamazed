{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Geo.Discrete.Bresenham3
    ( bresenham3Length
    , bresenham3
    ) where

import           Imj.Prelude

import           Data.List( zip3 )


{- | Source: https://www.reddit.com/r/haskell/comments/14h4az/3d_functional_bresenham_algorithm/

With the following modifications :

* fix a bug when rise1 == rise2, rise1 > run and rise2 > run
* make it produce an infinite list that goes beyond the target
* replace 'div' by 'quot' when we know the integral is positive
* add bangs -}
bres :: Int -> Int -> Int -> [(Int, Int, Int)]
bres !run !rise1 !rise2
    | run < 0   = [(-x,  y,  z) | (x, y, z) <- bres (-run) rise1 rise2]
    | rise1 < 0 = [( x, -y,  z) | (x, y, z) <- bres run (-rise1) rise2]
    | rise2 < 0 = [( x,  y, -z) | (x, y, z) <- bres run rise1 (-rise2)]
    | rise1 > max run rise2 = [( x, y, z) | (y, x, z) <- bres rise1 run rise2]
    | rise2 > max run rise1 = [( x, y, z) | (z, x, y) <- bres rise2 run rise1]
    | run < rise1 = [( x, y, z) | (y, x, z) <-
                                bres rise1 run (assert (rise1 == rise2) rise2)]
    | otherwise = zip3 [0..]
                       (map fst $ iterate (step rise1) (0, run `quot` 2))
                       (map fst $ iterate (step rise2) (0, run `quot` 2))
    where
        step rise (y, err)
            | err' < 0 = (y + 1, err' + run)
            | otherwise  = (y, err')
            where err' = err - rise

-- | 3D version of the bresenham algorithm.
{-# INLINABLE bresenham3 #-}
bresenham3 :: (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int)]
bresenham3 (!x1, !y1, !z1) (!x2, !y2, !z2) =
    [(x1+x, y1+y, z1+z) | (x, y, z) <- bres (x2-x1) (y2-y1) (z2-z1)]


-- | Returns the 3D bresenham length between two 3D coordinates.
{-# INLINABLE bresenham3Length #-}
-- avoid using unsigned types, as it complicates the calculations
bresenham3Length :: (Int, Int, Int) -> (Int, Int, Int) -> Int
bresenham3Length (x1, y1, z1) (x2, y2, z2)
  = succ $ max (abs (x1-x2)) $ max (abs (y1-y2)) (abs (z1-z2))
