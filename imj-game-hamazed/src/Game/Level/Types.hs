
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Level.Types
    ( Level(..)
    , LevelFinished(..)
    , MessageState(..)
    , GameStops(..)
    , firstLevel
    , lastLevel
    ) where


import           Imajuscule.Prelude

import           Timing

data Level = Level {
    _levelNumber :: !Int
  , _levelTarget :: !Int
  , _levelStatus :: !(Maybe LevelFinished)
}

data LevelFinished = LevelFinished {
    _levelFinishedResult :: !GameStops
  , _levelFinishedWhen :: !SystemTime
  , _levelFinishedCurrentMessage :: !MessageState
}

data MessageState = InfoMessage
                  | ContinueMessage
                  deriving(Eq, Show)

data GameStops = Lost Text
               | Won


lastLevel :: Int
lastLevel = 12

firstLevel :: Int
firstLevel = 1
