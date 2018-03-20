{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Game.Hamazed.Level.Types
    ( LevelSpec(..)
    , LevelEssence(..)
    , mkLevelEssence
    , mkEmptyLevelEssence
    , Level(..)
    , mkLevel
    , LevelTarget(..)
    , TargetConstraint(..)
    , initialLaserAmmo
    , LevelOutcome(..)
    , firstLevel
    , lastLevel
    ) where


import           Imj.Prelude
import           Control.DeepSeq(NFData)

initialLaserAmmo :: Int
initialLaserAmmo = 10

data Level = Level {
    _levelSpec :: {-# UNPACK #-} !LevelEssence
  , getLevelOutcome' :: {-unpack sum-} !(Maybe LevelOutcome)
} deriving (Generic)

mkLevel :: LevelEssence -> Level
mkLevel = flip Level Nothing

data LevelEssence = LevelEssence {
    getLevelNumber' :: {-# UNPACK #-} !Int
    -- ^ From 1 to 12
  , _levelTarget :: {-unpack sum-} !LevelTarget
  , _levelFlyingNumbers :: ![Int]
} deriving (Generic, Binary, NFData, Show)

data LevelTarget = LevelTarget {-# UNPACK #-} !Int {-unpack sum-} !TargetConstraint
  deriving (Generic, Binary, NFData, Show)

data TargetConstraint =
    CanOvershoot
  | CannotOvershoot
  deriving (Generic, Binary, NFData, Show)

data LevelSpec = LevelSpec {
    levelNumber :: {-# UNPACK #-} !Int
  , _targetConstraint :: {-unpack sum-} !TargetConstraint
} deriving(Generic, Show, NFData, Binary)


mkEmptyLevelEssence :: LevelEssence
-- we don't use 0 a target else the level would be won immediately.
mkEmptyLevelEssence = LevelEssence 0 (LevelTarget 1 CannotOvershoot) []

mkLevelEssence :: LevelSpec -> LevelEssence
mkLevelEssence (LevelSpec n co) =
  let numbers = [1..(3+n)] -- more and more numbers as level increases
      target = sum numbers `quot` 2
  in LevelEssence n (LevelTarget target co) numbers

data LevelOutcome =
    Lost !Text
   -- ^ 'Text' is the reason why the 'Level' was lost.
  | Won
  deriving(Generic, Binary, Eq, Show, NFData)

-- | 12
lastLevel :: Int
lastLevel = 12

-- | 1
firstLevel :: Int
firstLevel = 1
