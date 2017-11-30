{-# LANGUAGE NoImplicitPrelude #-}

module Math
    ( invPolyEaseInOut
    ) where

import           Imajuscule.Prelude

-- | returns the time (in range [0 1]) at which a value (in range [0 1]) is reached
-- given an nth order ease in-out function
invPolyEaseInOut :: Int -- ^ order
                 -> Float -- ^ value
                 -> Float -- ^ time
invPolyEaseInOut order y' =
  if y < 1
    then
      (recip (y * 2)) ** exponent
    else
      2 + (recip (2 * negate y) + 2) ** exponent
 where
   y = y' * 2
   exponent = fromIntegral $ negate order
