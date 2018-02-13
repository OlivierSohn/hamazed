{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Game.Hamazed.Level.Types
    ( Level(..)
    , LevelSpec(..)
    , mkLevelSpec
    , mkEmptyLevelSpec
    , LevelFinished(..)
    , MessageState(..)
    , LevelOutcome(..)
    , firstLevel
    , lastLevel
    ) where


import           Imj.Prelude
import           Control.DeepSeq(NFData)

import           Imj.Timing

data Level = Level {
    _levelSpec :: !LevelSpec
  , _levelStatus :: !(Maybe LevelFinished)
} deriving (Generic)

data LevelSpec = LevelSpec {
    getLevelNumber' :: !Int
    -- ^ From 1 to 12
  , _levelTarget :: !Int
  , _levelFlyingNumbers :: ![Int]
} deriving (Generic, Binary, NFData, Show)

mkEmptyLevelSpec :: LevelSpec
mkEmptyLevelSpec = LevelSpec 0 0 []

mkLevelSpec :: Int -> LevelSpec
mkLevelSpec levelNumber =
  let numbers = [1..(3+levelNumber)] -- more and more numbers as level increases
      target = sum numbers `quot` 2
  in LevelSpec levelNumber target numbers

data LevelFinished = LevelFinished {
    _levelFinishedResult :: !LevelOutcome
    -- ^ Lost or won
  , _levelFinishedWhen :: !(Time Point System)
  , _levelFinishedCurrentMessage :: !MessageState
} deriving (Generic)

data MessageState = InfoMessage
                  | ContinueMessage
                  deriving(Generic, Binary, Eq, Show)

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
