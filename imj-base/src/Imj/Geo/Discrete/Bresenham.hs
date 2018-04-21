{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Geo.Discrete.Bresenham
    ( bla
    , blaLength
    ) where

import           Imj.Prelude

-- adapted from http://www.roguebasin.com/index.php?title=Bresenham%27s_Line_Algorithm#Haskell

-- | Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
bla :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bla (!x0, !y0) (!x1, !y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (!p, !q, !step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                      | otherwise       = (abs dx, abs dy, yxStep)
      balancedWord :: Int -> [Int]
      balancedWord !eps
        | qty < 0   = 0 : balancedWord (eps + p)
        | otherwise = 1 : balancedWord qty
        where !qty = eps + p - q

      walk (w:ws) xy = xy : walk ws (step w xy)
      walk [] _ = error "logic"
  in  walk (balancedWord 0) (x0, y0)

blaLength :: (Int, Int) -> (Int, Int) -> Int
blaLength (r1,c1) (r2,c2) =
  succ $ max (abs (r1-r2)) $ abs (c1-c2)
