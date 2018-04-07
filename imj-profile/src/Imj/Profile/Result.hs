{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Profile.Result
    ( TestResult(..)
    , TestDuration(..)
    , TestDurations(..)
    , mkTestDurations
    , dispersion
    , SeedNumber(..)
    ) where

import           Imj.Prelude
import           Prelude(sqrt)
import           Data.List(foldl', length)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

import           Imj.Timing
import           Imj.Data.Class.Quantifiable
data TestResult a =
    Cancelled
  | SomeTimeout !Int
      -- how many timeouts, in that case we intentionally don't store the non-timeouts,
      -- to not draw conclusions from an incomplete time series.
  | Finished !(TestDurations a)
  deriving(Show)
-- Eq and Ord are based on mean durations.
instance Eq (TestResult a) where
  Cancelled == Cancelled = True
  Finished x == Finished y = x == y
  SomeTimeout n == SomeTimeout n' = n == n'
  _ == _ = False
instance Ord (TestResult a) where
  compare (SomeTimeout x) (SomeTimeout y) = compare x y
  compare (Finished x) (Finished y) = compare x y
  compare Cancelled Cancelled = EQ
  compare Cancelled _ = LT
  compare (SomeTimeout _) _ = GT
  compare (Finished _) _ = LT
  {-# INLINABLE compare #-}

newtype SeedNumber = SeedNumber Int
  deriving(Show, Ord, Eq, Real, Num, Enum, Integral)

-- | By seed
data TestDurations a = TD {
    testDurations :: !(Map SeedNumber (TestDuration a))
  , mean :: !(Time Duration System)
} deriving(Show)
instance Monoid (TestDurations a) where
  mempty = TD Map.empty zeroDuration
  mappend (TD l _) (TD l' _) = mkTestDurations $ Map.unionWith (error "would overwrite") l l'
  mconcat = mkTestDurations . Map.unionsWith (error "would overwrite") . map testDurations
-- Eq and Ord are based on mean durations.
instance Eq (TestDurations a) where
  x == y = mean x == mean y
instance Ord (TestDurations a) where
  compare x y = compare (mean x) (mean y)
  {-# INLINABLE compare #-}

-- dispersion is normalized standard deviation (normalized in the sense that the mean is mapped to 1)
dispersion :: TestDurations a -> Float
dispersion (TD m meanD) = go (Map.elems m)
 where
  go l
   | len == 0 = 0
   | f == 0 = 0
   | otherwise = sqrt variance
   where
    f = writeFloat meanD
    ratio = 1 / f
    normalizedDs = map ((* ratio) . writeFloat . testDuration) l
    sqSum = foldl' (\s d -> (d-1)*(d-1) + s) 0 normalizedDs
    len = length l
    variance = sqSum / fromIntegral len


data TestDuration a = TestDuration {
    testDuration :: !(Time Duration System)
  , testResult :: a
} deriving(Show)

mkTestDurations :: Map SeedNumber (TestDuration a) -> TestDurations a
mkTestDurations l = TD l $ average $ map testDuration $ Map.elems l
