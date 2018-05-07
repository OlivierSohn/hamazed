{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveLift #-}

module Imj.Data.AlmostFloat
         ( almost
         , AlmostFloat
         , almostDistance
         , mapNormalizedToDiscrete
         ) where

import           Imj.Prelude
import           Control.DeepSeq(NFData)
import           Data.Binary(Binary)
import           Data.List
import           Numeric(showFFloat)

almost :: Float -> AlmostFloat
almost = AlmostFloat

eps :: Float
eps = 1e-6

newtype AlmostFloat = AlmostFloat Float
 deriving(Generic, Floating, Fractional, Num, RealFrac, Real, Lift)
instance NFData AlmostFloat
instance Binary AlmostFloat
instance Eq AlmostFloat where
  (AlmostFloat x) == (AlmostFloat y) = abs (x-y) < eps
instance Ord AlmostFloat where
  a@(AlmostFloat x) `compare` a'@(AlmostFloat y)
    | a == a' = EQ
    | otherwise = x `compare` y
instance Show AlmostFloat where
  show (AlmostFloat f) =
    trimZeros str
   where
    str = showFFloat (Just 6) f ""
    trimZeros = bool id (reverse . dropWhile (=='0') . reverse) $ '.' `elem` str

almostDistance :: AlmostFloat -> AlmostFloat -> Float
almostDistance a@(AlmostFloat f) a'@(AlmostFloat f')
  | a == a' = 0
  | otherwise = abs(f-f')

{-# INLINE mapNormalizedToDiscrete #-}
mapNormalizedToDiscrete :: (Integral a) => AlmostFloat -> a -> a
mapNormalizedToDiscrete (AlmostFloat f) one =
  round $ f * fromIntegral one
