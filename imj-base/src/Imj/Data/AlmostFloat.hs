{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Data.AlmostFloat
         ( almost
         , AlmostFloat
         ) where

import           Imj.Prelude
import           Data.List
import           Numeric(showFFloat)

almost :: Float -> AlmostFloat
almost = AlmostFloat

eps :: Float
eps = 1e-6

newtype AlmostFloat = AlmostFloat Float
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
