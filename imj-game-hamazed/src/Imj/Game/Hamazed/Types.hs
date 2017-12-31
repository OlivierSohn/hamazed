{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Types
    ( GameState(..)
    -- * Reexports
    , module Imj.Game.Hamazed.Level.Types
    , module Imj.Game.Hamazed.World.Types
    , UIAnimation
    ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.World.Types
import           Imj.Graphics.UI.Animation


data GameState = GameState {
    _gameStateNextMotionStep :: !(Maybe KeyTime)
    -- ^ When the next 'World' motion update should happen,
  , _gameStatePreviousWorld :: !World
    -- ^ The previous 'World'
  , _gameStateCurrentWorld :: !World
    -- ^ The current 'World'
  , _gameStateShotNumbers :: ![Int]
    -- ^ Which 'Number's were shot
  , _gameStateLevel :: !Level
    -- ^ The current 'Level'
  , _gameStateUIAnimation :: !UIAnimation
    -- ^ Inter-level animation.
}