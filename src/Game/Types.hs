
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Types
    ( GameState(..)
    -- | reexports
    , module Game.World.Types
    , module Game.Level.Types
    , module Timing
    ) where

import           Imajuscule.Prelude

import           Game.World.Types
import           Game.Level.Types

import           Timing


data GameState = GameState {
    _gameStateStartTime :: !Timer
  , _gameStateNextMotionStep :: !(Maybe KeyTime)
  , _gameStateWorld :: !World
  , _gameStateShotNumbers :: ![Int]
  , _gameStateLevel :: !Level
  , _gameStateWorldAnimation :: !WorldAnimation
}
