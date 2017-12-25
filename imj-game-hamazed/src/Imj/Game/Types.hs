
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Types
    ( GameState(..)
    -- * Reexports
    , module Imj.Game.World.Types
    , module Imj.Game.Level.Types
    , module Imj.Game.Timing
    ) where

import           Imj.Prelude

import           Imj.Game.World.Types
import           Imj.Game.Level.Types

import           Imj.Game.Timing


data GameState = GameState {
    _gameStateNextMotionStep :: !(Maybe KeyTime)
  , _gameStateWorld :: !World
  , _gameStateNextWorld :: !World
  , _gameStateShotNumbers :: ![Int]
  , _gameStateLevel :: !Level
  , _gameStateWorldAnimation :: !WorldAnimation
}
