{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Util
    (
    -- * Text manipulation
      maxOneSpace
    -- * Math utilities
    , clamp
    , zigzag
    , lastAbove
    , logBase2
    , ceilToMultiple
    , mapRange
    , unsafeMapRange
    -- * Reexports
    , Int64
    ) where

import           Imj.Prelude

import           Data.Bits(finiteBitSize, countLeadingZeros)
import           Data.Char(isSpace)
import           Data.Int(Int64)
import           Data.Text(pack, unpack)

-- | Removes spaces on extremities and converts every inner consecutive spaces to a single space.
maxOneSpace :: Text -> Text
maxOneSpace t = pack $ go (unpack t) False []
 where
  go [] _ res = reverse res
  go (c:rest) prevSpace res
    | isSpace c = go rest (not $ null res) res
    | prevSpace = go rest False $ c:' ':res
    | otherwise = go rest False $ c:res


-- | Produces an infinite triangle signal given a linear input.
{-# INLINABLE zigzag #-}
zigzag :: Integral a
       => a
       -- ^ Inclusive min
       -> a
       -- ^ Inclusive max
       -> a
       -- ^ Value
       -> a
zigzag from' to' v =
  let from = min from' to'
      to = max from' to'
      d = to-from
      v' = v `mod` (2*d)
  in from + if v' <= d
              then v'
              else
                2*d - v'

{-# INLINABLE mapRange #-}
mapRange :: (Fractional a, Eq a)
         => a
         -- ^ low 1
         -> a
         -- ^ high 1
         -> a
         -- ^ low 2
         -> a
         -- ^ high 2
         -> a
         -- ^ value 1
         -> Maybe a
         -- ^ value 2
mapRange l1 h1 l2 h2 v1
  | denom == 0 = Nothing
  | otherwise = Just $ l2 + normalized * (h2 - l2)
 where
  denom = h1 - l1
  normalized = (v1 - l1) / denom

-- | same as 'mapRange' except that the denomiator is not checked for 0 before dividing.
unsafeMapRange :: (Fractional a)
         => a
         -- ^ low 1
         -> a
         -- ^ high 1
         -> a
         -- ^ low 2
         -> a
         -- ^ high 2
         -> a
         -- ^ value 1
         -> a
         -- ^ value 2
unsafeMapRange l1 h1 l2 h2 v1 =
  l2 + normalized * (h2 - l2)
 where
  denom = h1 - l1
  normalized = (v1 - l1) / denom

-- | Expects the bounds to be in the right order.
{-# INLINABLE clamp #-}
clamp :: Ord a
      => a
      -- ^ The value
      -> a
      -- ^ The inclusive minimum bound
      -> a
      -- ^ The inclusive maximum bound
      -> a
clamp !n min_ max_
  | n < min_ = min_
  | n > max_ = max_
  | otherwise = n

-- | Inputs are expected to be positive.
{-# INLINABLE ceilToMultiple #-}
ceilToMultiple :: (Integral a)
               => a
               -- ^ The multiple
               -> a
               -- ^ The value to ceil
               -> a
ceilToMultiple multiple value
  | r > 0     = multiple * (q+1)
  | otherwise = multiple * q
 where
  (q,r) = value `quotRem` multiple


{- | Given :

* a discrete input interval @[i,j]@
* a /decreasing/ monadic function @f :: (Ord y) => Int -> m y@
* an output /threshold/ value @y@

finds @x@ in @[i,j]@ such that :

* @f(x) > y@, and
* either @y >= f(x+1)@ or @x == j@

With a time complexity of O(log(j-i)).
-}
lastAbove :: (Monad m
            , Integral x
            , Ord y)
          => y
          -- ^ Output value
          -> (x -> m y)
          -- ^ Decreasing function
          -> x
          -- ^ Inclusive min bound
          -> x
          -- ^ Inclusive max bound
          -> m (Maybe x)
lastAbove threshold f minIdx maxIdx =
  go (pred minIdx) (succ maxIdx) Nothing -- TODO refactor to first test extremities?
 where
  go tooLow tooHigh res
    -- stop when the admissible range is empty:
    | tooLow >= pred tooHigh = return res
    | otherwise = f x >>= \middleValue ->
        if middleValue > threshold
          then
            -- adjust the "best" index found, adjust the lower bound to that index and continue the search.
            go x tooHigh $ Just $ maybe x (max x) res
          else
            -- adjust the upper bound to that index (we assume that
            -- if 'condition i' is not satisfied, then for every j > i, condition j is not satisfied)
            -- and continue the search.
            go tooLow x res
      where
        x = quot (tooLow + tooHigh) 2

{-# INLINE logBase2 #-}
logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x
