{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Profile.Result
    ( TestStatus(..)
    , mkStatus
    , mkEmptyStatus
    , TestDurations(..)
    , Summary(..)
    , getSummaryDuration
    , summarize
    , Dispersion(..)
    , SeedNumber(..)
    , unSeedNumber
    ) where

import           Imj.Prelude
import           Prelude(sqrt)
import           Data.List(foldl', length)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

import           Imj.Timing
import           Imj.Data.Class.Quantifiable

data Summary =
    NTimeouts !Int
  | FinishedAverage !(Time Duration System) !Dispersion -- NOTE Dispersion is ignored in 'Ord' and 'Eq' instances
  | NoResult
-- | 'Eq' and 'Ord' instances are based on durations : 'FinishedAverage' < 'NTimeouts' < 'NoResult'
instance Eq Summary where
  NoResult == NoResult = True
  FinishedAverage x _ == FinishedAverage y _ = x == y
  NTimeouts x == NTimeouts y = x == y
  _ == _ = False
-- | 'Eq' and 'Ord' instances are based on durations : 'FinishedAverage' < 'NTimeouts' < 'NoResult'
instance Ord Summary where
  compare (NTimeouts x) (NTimeouts y) = compare x y
  compare (FinishedAverage x _) (FinishedAverage y _) = compare x y
  compare NoResult NoResult = EQ
  compare NoResult _ = LT
  compare (NTimeouts _) _ = GT
  compare (FinishedAverage _ _) _ = LT
  {-# INLINABLE compare #-}

getSummaryDuration :: Summary -> Maybe (Time Duration System)
getSummaryDuration = \case
  NTimeouts _ -> Nothing
  NoResult -> Nothing
  FinishedAverage dt _ -> Just dt

data TestStatus a = -- TODO remove maybe unused Ord, Eq
    NotStarted
  | Finished !(Time Duration System) !a
  | Timeout
  deriving(Generic, Show)
-- | 'Eq' and 'Ord' instances are based on durations : 'Finished' < ' Timeout' < 'NotStarted'
instance (Eq a) => Eq (TestStatus a) where
  NotStarted == NotStarted = True
  Finished x _ == Finished y _ = x == y
  Timeout == Timeout = True
  _ == _ = False
-- | 'Eq' and 'Ord' instances are based on durations : 'Finished' < ' Timeout' < 'NotStarted'
instance (Ord a) => Ord (TestStatus a) where
  compare Timeout Timeout = EQ
  compare (Finished x _) (Finished y _) = compare x y
  compare NotStarted NotStarted = EQ
  compare NotStarted _ = LT
  compare Timeout _ = GT
  compare (Finished _ _) _ = LT
  {-# INLINABLE compare #-}
instance (Binary a) => Binary (TestStatus a)

mkStatus :: Maybe (Time Duration System, a) -> TestStatus a
mkStatus = maybe Timeout (uncurry Finished)

mkEmptyStatus :: TestStatus a
mkEmptyStatus = NotStarted

newtype SeedNumber = SeedNumber Int
  deriving(Show, Ord, Eq, Real, Num, Enum, Integral, Binary)
unSeedNumber :: SeedNumber -> Int
unSeedNumber (SeedNumber s) = s

-- | By seed
newtype TestDurations k a = TD (Map k (TestStatus a))
  deriving(Show, Binary)
instance (Ord k) => Monoid (TestDurations k a) where
  mempty = TD Map.empty
  mappend (TD l) (TD l') = TD $ Map.unionWith (error "would overwrite") l l'
  mconcat = TD . Map.unionsWith (error "would overwrite") . map (\(TD m) -> m)

-- | Ignoring tests that didn't run yet:
-- if one test is Timeout, return the timeout value,
-- else return the average of finished tests.
summarize :: TestDurations k a -> Summary
summarize (TD d)
 | nTimeouts == 0 = case Map.elems $ Map.mapMaybe (\case Finished dt _ -> Just dt; _ -> Nothing) d of
    [] -> NoResult
    xs@(_:_) -> FinishedAverage (average xs) $ mkDispersion xs
 | otherwise = NTimeouts nTimeouts
  where
   nTimeouts = Map.size $ Map.filter (\case Timeout -> True; _ -> False) d

-- Dispersion is normalized standard deviation (normalized in the sense that the mean is mapped to 1)
newtype Dispersion = Dispersion Float
  deriving(Num, Real, Ord, Eq, Fractional)

mkDispersion :: (Quantifiable a) => [a] -> Dispersion
mkDispersion l
 | len == 0 = 0
 | f == 0 = 0
 | otherwise = Dispersion $ sqrt variance
 where
  meanD = average l
  f = writeFloat meanD
  ratio = 1 / f
  normalizedDs = map ((* ratio) . writeFloat) l
  sqSum = foldl' (\s d -> (d-1)*(d-1) + s) 0 normalizedDs
  len = length l
  variance = sqSum / fromIntegral len
