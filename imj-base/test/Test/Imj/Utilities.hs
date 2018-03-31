{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Utilities
         ( almost
         , AlmostFloat
         ) where

import           Imj.Prelude

almost :: Float -> AlmostFloat
almost = AlmostFloat

newtype AlmostFloat = AlmostFloat Float
  deriving(Show)
instance Eq AlmostFloat where
  (AlmostFloat x) == (AlmostFloat y) = abs (x-y) < 1e-6
