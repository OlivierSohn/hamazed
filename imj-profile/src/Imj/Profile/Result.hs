{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Profile.Result
    ( TestResult(..)
    , TestDuration(..)
    , TestDurations(..)
    , mkTestDurations
    ) where

import           Imj.Prelude

import           Imj.Timing
import           Imj.Data.Class.Quantifiable
data TestResult a =
    Cancelled
  | SomeTimeout !Int !(Time Duration System)
      -- how many timeouts, in that case we intentionally don't store the non-timeouts,
      -- to not draw conclusions from an incomplete time series.
  | Finished !(TestDurations a)
  deriving(Show)

data TestDuration a = TestDuration {
    testDuration :: !(Time Duration System)
  , testResult :: a
} deriving(Show)

data TestDurations a = TD {
    testDurations :: ![TestDuration a]
  , mean :: !(Time Duration System)
} deriving(Show)
instance Monoid (TestDurations a) where
  mempty = TD [] zeroDuration
  mappend (TD l _) (TD l' _) = mkTestDurations $ l <> l'
  mconcat = mkTestDurations . concatMap testDurations

mkTestDurations :: [TestDuration a] -> TestDurations a
mkTestDurations l = TD l $ average $ map testDuration l
