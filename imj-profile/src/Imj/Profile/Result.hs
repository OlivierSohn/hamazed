{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Profile.Result
    ( TestResult(..)
    , TestDuration(..)
    , TestDurations(..)
    , mkTestDurations
    , dispersion
    ) where

import           Imj.Prelude
import           Prelude(sqrt)
import           Data.List(foldl', length)

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

data TestDurations a = TD {
    testDurations :: ![TestDuration a]
  , mean :: !(Time Duration System)
} deriving(Show)
instance Monoid (TestDurations a) where
  mempty = TD [] zeroDuration
  mappend (TD l _) (TD l' _) = mkTestDurations $ l <> l'
  mconcat = mkTestDurations . concatMap testDurations
-- Eq and Ord are based on mean durations.
instance Eq (TestDurations a) where
  x == y = mean x == mean y
instance Ord (TestDurations a) where
  compare x y = compare (mean x) (mean y)
  {-# INLINABLE compare #-}

-- dispersion is normalized standard deviation (normalized in the sense that the mean is mapped to 1)
dispersion :: TestDurations a -> Float
dispersion (TD [] _) = 0
dispersion (TD ds meanD)
  | f == 0 = 0
  | otherwise = sqrt variance
 where
  f = writeFloat meanD
  ratio = 1 / f
  normalizedDs = map ((* ratio) . writeFloat . testDuration) ds
  sqSum = foldl' (\s d -> (d-1)*(d-1) + s) 0 normalizedDs
  variance = sqSum / fromIntegral (length ds)


data TestDuration a = TestDuration {
    testDuration :: !(Time Duration System)
  , testResult :: a
} deriving(Show)

mkTestDurations :: [TestDuration a] -> TestDurations a
mkTestDurations l = TD l $ average $ map testDuration l
