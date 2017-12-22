{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Ease
    (
    -- * (Inverse) Easing
      invQuartEaseInOut
    -- ** Adapt to discrete values
    , discreteAdaptor
    , discreteInvQuartEaseInOut
    ) where

import           Imj.Prelude

{- |
Returns the time (in range [0 1]) at which a value (in range [0 1]) is reached
given a 4th order ease in-out function.

This is the equation we want to invert:

@
quartInOut :: Float -> Float
quartInOut time =
    if time < 0.5
    then        1 / 2 *  2^4 * time  * time  * time  * time
    else negate 1 / 2 * (2^4 * (time-1) * (time-1) * (time-1) * (time-1) - 2)
@

These are the successive transformations that lead to invQuartEaseInOut implementation
(note that there are multiple solutions, we chose the one that produces results in [0,1] range):

@
y < 0.5 :
  y = 2^3 * time  * time  * time  * time
  y / (2^3) = time^4
  (y \/ (2^3))^(1 / 4) = time
y > 0.5 :
  y = - 1 / 2 *(2^4 * (time-1) * (time-1) * (time-1) * (time-1) - 2)
  -2*y = (2^4 * (time-1) * (time-1) * (time-1) * (time-1) - 2)
  2-2*y = 2^4 * (time-1)^4
  1-y = 2^3 * (time-1)^4
  (1-y) / (2^3) = (time-1)^4
  ((1-y) \/ (2^3))^(1 / 4) = (1-time)
  1-((1-y) \/ (2^3))^(1 / 4) = time
@
 -}
invQuartEaseInOut :: Float
                  -- ^ Value
                  -> Float
                  -- ^ Time
invQuartEaseInOut y =
  if y < 0.5
    then
      (y / 8.0) ** (1.0/4.0)
    else
      1.0 - ((1.0 - y) / 8.0) ** (1.0/4.0)

-- | Adapts continuous inout ease functions to the discrete case.
discreteAdaptor :: (Float -> Float)
                -- ^ Continuous (optionally inverse) ease in/out function
                -> Int
                -- ^ The number of discrete steps
                -> Float
                -- ^ Input value
                -> Float
                -- ^ (optionnaly inverse) Eased value
discreteAdaptor f n v =
  let nIntervals = n
      intervalSize = recip $ fromIntegral nIntervals
      firstValue = intervalSize / 2
      lastValue = 1 - firstValue
      scaledValue = firstValue + v * (lastValue - firstValue)
  in f scaledValue

discreteInvQuartEaseInOut :: Int
                          -> Float
                          -> Float
discreteInvQuartEaseInOut = discreteAdaptor invQuartEaseInOut
