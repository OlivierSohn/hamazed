
{-# LANGUAGE NoImplicitPrelude #-}

module Geo.Discrete.Bresenham3
    ( bresenham3Length
    , bresenham3
    ) where

import           Imajuscule.Prelude

import           Data.List( zip3 )

-- source: https://www.reddit.com/r/haskell/comments/14h4az/3d_functional_bresenham_algorithm/
-- slightly modified to fix a bug when rise1 == rise2, rise1 > run and rise2 > run
bres :: Int -> Int -> Int -> [(Int, Int, Int)]
bres run rise1 rise2
    | run < 0  =   [(-x,  y,  z) | (x, y, z) <- bres (-run) rise1 rise2]
    | rise1 < 0  = [( x, -y,  z) | (x, y, z) <- bres run (-rise1) rise2]
    | rise2 < 0  = [( x,  y, -z) | (x, y, z) <- bres run rise1 (-rise2)]
    | rise1 > max run rise2 =
        [( x, y, z) | (y, x, z) <- bres rise1 run rise2]
    | rise2 > max run rise1 =
        [( x, y, z) | (z, x, y) <- bres rise2 run rise1]
    | run < rise1 =
        [( x, y, z) | (y, x, z) <- bres rise1 run (assert (rise1 == rise2) rise2)]
    | otherwise = zip3 [0..run]
                       (map fst $ iterate (step rise1) (0, run `div` 2))
                       (map fst $ iterate (step rise2) (0, run `div` 2))
    where
        step rise (y, err)
            | err' < 0 = (y + 1, err' + run)
            | otherwise  = (y, err')
            where err' = err - rise

{-# INLINABLE bresenham3 #-}
bresenham3 :: (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int)]
bresenham3 (x1, y1, z1) (x2, y2, z2) =
    [(x1+x, y1+y, z1+z) | (x, y, z) <- bres (x2-x1) (y2-y1) (z2-z1)]


{-# INLINABLE bresenham3Length #-}
-- avoid using unsigned types, as it complicates the calculations
bresenham3Length :: (Int, Int, Int) -> (Int, Int, Int) -> Int
bresenham3Length (x1, y1, z1) (x2, y2, z2)
  = succ $ max (abs (x1-x2)) $ max (abs (y1-y2)) (abs (z1-z2))
