{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Types
    ( Game(..)
    , GameTime
    , UserIntent(..)
    , GameState(..)
    , GameParameters(..)
    , initialParameters
    , minRandomBlockSize
    -- * Reexports
    , module Imj.Game.Hamazed.Level.Types
    , module Imj.Game.Hamazed.World.Types
    , UIAnimation
    ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.World.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.UI.Animation

data UserIntent = Configure
                | Play

data Game = Game {
    _gameUserIntent :: !UserIntent
  , _gameGameParameters :: !GameParameters
  , _gameGameState :: !GameState
}

data GameParameters = GameParameters {
    _gameParamsWorldShape :: !WorldShape
  , _gameParamsWallDistrib :: !WallDistribution
  , _gameParamsViewMode :: !ViewMode
}

{-| 'GameState' has two fields of type 'World' : during 'Level' transitions,
we draw the /old/ 'World' while using the /new/ 'World' 's
dimensions to animate the UI accordingly. -}
data GameState = GameState {
    _gameStateNextMotionStep :: !(Maybe (Time Point System))
    -- ^ When the next 'World' motion update should happen
  , _gameStateTimeMultiplicator :: !(Multiplicator GameTime)
  , _gameStatePreviousWorld :: !World
    -- ^ The previous 'World'
  , gameStateCurrentWorld :: !World
    -- ^ The current 'World'
  , _gameStateShotNumbers :: ![Int]
    -- ^ Which 'Number's were shot
  , _gameStateLevel :: !Level
    -- ^ The current 'Level'
  , _gameStateUIAnimation :: !UIAnimation
    -- ^ Inter-level animation.
  , _gameStateScreen :: !Screen
}


minRandomBlockSize :: Int
minRandomBlockSize = 6 -- using 4 it once took a very long time (one minute, then I killed the process)
                       -- 6 has always been ok

initialParameters :: GameParameters
initialParameters = GameParameters Rectangle2x1 (Random defaultRandom) CenterSpace

defaultRandom :: RandomParameters
defaultRandom = RandomParameters minRandomBlockSize StrictlyOneComponent
