{-# LANGUAGE NoImplicitPrelude #-}

module Math
    ( invQuartEaseInOut
    , clamp
    ) where

import           Imajuscule.Prelude

{--
This is the equation we want to invert:

quartInOut :: Ord a => Ease a
quartInOut time =
    if time < 0.5
    then        1 / 2 *  2^4 * time  * time  * time  * time
    else negate 1 / 2 * (2^4 * (time-1) * (time-1) * (time-1) * (time-1) - 2)

These are the successive transformations that lead to invQuartEaseInOut implementation
(note that there are multiple solutions, we chose the one that produces results in [0,1] range):

y < 0.5 :
  y = 2^3 * time  * time  * time  * time
  y/(2^3) = time^4
  (y/(2^3))^(1/4) = time
y > 0.5 :
  y = - 1 / 2 *(2^4 * (time-1) * (time-1) * (time-1) * (time-1) - 2)
  -2*y = (2^4 * (time-1) * (time-1) * (time-1) * (time-1) - 2)
  2-2*y = 2^4 * (time-1)^4
  1-y = 2^3 * (time-1)^4
  (1-y)/(2^3) = (time-1)^4
  ((1-y)/(2^3))^(1/4) = (1-time)
  1-((1-y)/(2^3))^(1/4) = time
--}

-- | returns the time (in range [0 1]) at which a value (in range [0 1]) is reached
-- given an nth order ease in-out function
invQuartEaseInOut :: Float -- ^ value
                  -> Float -- ^ time
invQuartEaseInOut y =
  if y < 0.5
    then
      (y / 8.0) ** (1.0/4.0)
    else
      1.0 - ((1.0 - y) / 8.0) ** (1.0/4.0)

clamp :: Int
      -- ^ the value
      -> Int
      -- ^ the inclusive minimum bound
      -> Int
      -- ^ the inclusive maximum bound
      -> Int
clamp n min_ max_
  | n < min_ = min_
  | n > max_ = max_
  | otherwise = n
