
{-# LANGUAGE NoImplicitPrelude #-}

module Level.Types
    ( Level(..)
    , LevelFinished(..)
    , MessageState(..)
    , GameStops(..)
    ) where


import           Imajuscule.Prelude

import           Timing

data Level = Level {
    _levelNumber :: !Int
  , _levelStatus :: !(Maybe LevelFinished)
}

data LevelFinished = LevelFinished {
    _levelFinishedResult :: !GameStops
  , _levelFinishedWhen :: !UTCTime
  , _levelFinishedCurrentMessage :: !MessageState
}

data MessageState = InfoMessage
                  | ContinueMessage
                  deriving(Eq, Show)

data GameStops = Lost Text
               | Won
