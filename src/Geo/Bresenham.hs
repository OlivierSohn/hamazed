
{-# LANGUAGE NoImplicitPrelude #-}

module Geo.Bresenham
    ( bresenhamLength
    , bresenham
    ) where

import           Imajuscule.Prelude

import           Geo.Types
import           Util( takeWhileInclusive
                     , range )

bresenhamLength :: Coords -> Coords -> Int
bresenhamLength (Coords (Row r1) (Col c1)) (Coords (Row r2) (Col c2))
  = 1 + max (abs (r1-r2)) (abs (c1-c2))

bresenham :: Segment -> [Coords]
bresenham (Horizontal r c1 c2) = map (Coords r . Col) $ range c1 c2
bresenham (Vertical c r1 r2)   = map (flip Coords c . Row) $ range r1 r2
bresenham (Oblique (Coords (Row y0) (Col x0)) c2@(Coords (Row y1) (Col x1))) =
  takeWhileInclusive (/= c2) $ map (\(x,y) -> Coords (Row y) (Col x) ) $ bla (x0,y0) (x1,y1)

-- adapted from http://www.roguebasin.com/index.php?title=Bresenham%27s_Line_Algorithm#Haskell
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps
  | eps + p < q = 0 : balancedWord p q (eps + p)
  | otherwise   = 1 : balancedWord p q (eps + p - q)

-- | Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
bla :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bla (x0, y0) (x1, y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                   | otherwise       = (abs dx, abs dy, yxStep)
      walk w xy = xy : walk (tail w) (step (head w) xy)
  in  walk (balancedWord p q 0) (x0, y0)

-- source: https://www.reddit.com/r/haskell/comments/14h4az/3d_functional_bresenham_algorithm/
{--
bres run rise1 rise2
    | run < 0  =   [(-x,  y,  z) | (x, y, z) <- bres (-run) rise1 rise2]
    | rise1 < 0  = [( x, -y,  z) | (x, y, z) <- bres run (-rise1) rise2]
    | rise2 < 0  = [( x,  y, -z) | (x, y, z) <- bres run rise1 (-rise2)]
    | rise1 > (max run rise2) =
        [( x, y, z) | (y, x, z) <- bres rise1 run rise2]
    | rise2 > (max run rise1) =
        [( x, y, z) | (z, x, y) <- bres rise2 run rise1]
    | otherwise = zip3 [0..run]
                       (map fst $ iterate (step rise1) (0, run `div` 2))
                       (map fst $ iterate (step rise2) (0, run `div` 2))

    where
        step rise (y, error)
            | error' < 0 = (y + 1, error' + run)
            | otherwise  = (y, error')
            where error' = error - rise

line (x1, y1, z1) (x2, y2, z2) =
    [(x1+x, y1+y, z1+z) | (x, y, z) <- bres (x2-x1) (y2-y1) (z2-z1)]
--}
