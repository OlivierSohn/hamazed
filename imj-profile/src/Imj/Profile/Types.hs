{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Profile.Types
  ( TestProgress(..)
  , mkZeroProgress
  , updateProgress
  )
  where

import           Imj.Prelude hiding(div)

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.List as List
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.UUID(UUID)

import           Imj.Game.Hamazed.World.Space.Types

import           Imj.Profile.Result
import           Imj.Profile.Results
import           Imj.Random.MWC.Util
import           Imj.Timing

-- | Represents the progress of 'withTestScheduler''
data TestProgress = TestProgress {
    _uuid :: !UUID
    -- ^ Test unique identifier
  , _timeoutThisIteration :: !(Time Duration System)
  , _hintsStrategies :: !(Map (SmallWorldCharacteristics Program) (Maybe MatrixVariantsSpec))
   -- ^ Hints for the key of the fastest strategies
  , _allStrategies :: !(Set (Maybe MatrixVariantsSpec))
  , willTestInIteration :: ![[SmallWorldCharacteristics Program]]
   -- ^
   -- The outer list, when filtered on a single ComponentCount, is sorted by /increasing/ areas of the first 'SmallWorldCharacteristics' of the inner list.
   -- During the current iteration, we will skip the elements of the inner list that come after
   -- the first one that timeouts.
  , _willTestNextIteration :: ![[SmallWorldCharacteristics Program]]
  -- ^ The outer list is sorted by decreasing areas of the first 'SmallWorldCharacteristics' of the inner list
  -- These are elements of the inner lists that timeouted, or were located after an element that timeouted.
  , _profileResults :: !(MaybeResults (NonEmpty SeedNumber))
} deriving (Generic)
instance Binary TestProgress

mkZeroProgress :: [[SmallWorldCharacteristics Program]]
               -> Set (Maybe MatrixVariantsSpec)
               -> IO TestProgress
mkZeroProgress worlds strategies = do
  key <- randUUID
  return $ TestProgress key dt0 Map.empty strategies worlds [] (mkNothingResults $ Set.fromList $ concat worlds)
 where
  dt0 = fromSecs 0.0001

updateProgress :: Time Duration System
               -> Map (SmallWorldCharacteristics Program) (Maybe MatrixVariantsSpec)
               -> [[SmallWorldCharacteristics Program]]
               -> [[SmallWorldCharacteristics Program]]
               -> MaybeResults (NonEmpty SeedNumber)
               -> TestProgress
               -> TestProgress
updateProgress a b c d e (TestProgress uu _ _ st _ _ _) =
  TestProgress uu a b st c d e
