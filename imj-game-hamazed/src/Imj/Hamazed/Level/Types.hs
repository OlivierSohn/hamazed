{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Hamazed.Level.Types
    ( Level(..)
    , LevelFinished(..)
    , MessageState(..)
    , GameStops(..)
    , firstLevel
    , lastLevel
    ) where


import           Imj.Prelude

import           Imj.Timing

data Level = Level {
    _levelNumber :: !Int
    -- ^ From 1 to 12
  , _levelTarget :: !Int
    -- ^ The /target number/
  , _levelStatus :: !(Maybe LevelFinished)
}

data LevelFinished = LevelFinished {
    _levelFinishedResult :: !GameStops
    -- ^ Lost or won
  , _levelFinishedWhen :: !SystemTime
  , _levelFinishedCurrentMessage :: !MessageState
}

data MessageState = InfoMessage
                  | ContinueMessage
                  deriving(Eq, Show)

data GameStops = Lost Text
               -- ^ 'Text' is the reason why the 'Level' was lost.
               | Won

-- | 12
lastLevel :: Int
lastLevel = 12

-- | 1
firstLevel :: Int
firstLevel = 1
