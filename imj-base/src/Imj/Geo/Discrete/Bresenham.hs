{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Discrete.Bresenham
    ( bla
    ) where

import           Imj.Prelude


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
