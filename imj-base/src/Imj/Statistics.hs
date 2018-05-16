{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Statistics
    ( histogram
    , Histogram
    , Bar(..)
    ) where

import           Prelude as Unsafe(head, last)
import           Prelude(length)
import           Imj.Prelude
import           Data.List(sort, replicate, unwords, unlines)

import           Imj.Graphics.Text.Render

newtype Histogram a = Histogram [Bar a]

instance Show a => Show (Histogram a) where
  show (Histogram l) =
    unlines $
      map
        (\(Bar mi ma c) ->
          unwords ["|", justifyL nMax (replicate c '*'), show mi, show ma]) l
   where
    nMax = fromMaybe 0 $ maximumMaybe $ map _count l

data Bar a = Bar {
    _min, _max :: !a
  , _count :: !Int
}

histogram :: (Ord a, Fractional a)
          => [a]
          -- ^ Values
          -> Int
          -- ^ Number of equal-width consecutive bars in the histogram,
          -- where the min and max of the values are at the min of the first bar and max of the last bar.
          -> Histogram a
histogram [] _ = Histogram []
histogram _ 0 = Histogram []
histogram values n =

  Histogram $ go s upperBounds []

 where
  s = sort values
  min_ = Unsafe.head s
  max_ = Unsafe.last s
  width = (max_ - min_) / fromIntegral n
  upperBounds = map (\i -> min_ + fromIntegral i * width) [1..n-1] ++ [max_]

  go [] (upper:_) l = [Bar (upper - width) upper $ length l]
  go _ [] _ = error "logic"
  go sorted@(low:highs) us@(upper:uppers) barValues
    | low <= upper =
        go highs us (low:barValues)
    | otherwise =
        Bar (upper - width) upper (length barValues):
        go sorted uppers []
