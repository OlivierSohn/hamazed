{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Types
        ( BattleShip(..)
        , World(..)
        , Number(..)
        , BoundedAnimationUpdate(..)
        , Boundaries(..)
        , FrameSpec(..)
        , TextAnimSpec(..)
        , mkFrameSpec
        , WorldAnimation(..)
        , WorldEvolutions(..)
        , EmbeddedWorld(..)
        , isFinished
        -- * Reexports
        , module Game.World.Space.Types
        , module Game.World.Frame.Types
        , module Iteration
        , module Text.Animation
        , Terminal.Window
        ) where

import           Imajuscule.Prelude

import qualified System.Console.Terminal.Size as Terminal( Window(..))

import           Animation.Types

import           Game.World.Space.Types
import           Game.World.Frame.Types

import           Iteration

import           Text.Animation
import           Text.ColorString

import           Timing

data WorldAnimation = WorldAnimation {
    _worldAnimationEvs :: !WorldEvolutions
  , _worldAnimationDeadline :: !(Maybe KeyTime)
  , _worldAnimationProgress :: !Iteration
} deriving(Show)

data WorldEvolutions = WorldEvolutions {
    _worldEvolutionFrame :: !(Evolution FrameAnimationParallel4)
  , _worldEvolutionsUpDown :: !(TextAnimation AnchorChars)
  , _worldEvolutionLeft    :: !(TextAnimation AnchorStrings)
} deriving(Show)

isFinished :: WorldAnimation -> Bool
isFinished (WorldAnimation _ Nothing _) = True
isFinished _ = False

data TextAnimSpec = TextAnimSpec {
    _txtAnimSpecTexts :: ![ColorString]
  , _txtAnimSpecFrameSpec :: !FrameSpec
}

mkFrameSpec :: LayeredColor -> World m -> FrameSpec
mkFrameSpec colors (World _ _ _ (Space _ sz _) _ (EmbeddedWorld _ upperLeft)) =
  FrameSpec sz upperLeft colors

data World m = World {
    _worldNumbers :: ![Number]
  , _howBallMoves :: Space -> PosSpeed -> PosSpeed
  , _worldShip :: !BattleShip
  , _worldSpace :: !Space
  , _worldAnimations :: ![BoundedAnimationUpdate m]
  , _worldEmbedded :: !EmbeddedWorld
}

data EmbeddedWorld = EmbeddedWorld {
    _embeddedWorldTerminal :: !(Maybe (Terminal.Window Int))
  , _embeddedWorldUpperLeft :: !Coords
} deriving (Show)

data BoundedAnimationUpdate m = BoundedAnimationUpdate !(AnimationUpdate m) !Boundaries deriving(Show)

data Boundaries = WorldFrame
                | TerminalWindow
                | Both
                deriving(Show)


data BattleShip = BattleShip {
    _shipPosSpeed :: !PosSpeed
  , _shipAmmo :: !Int
  , _shipSafeUntil :: !(Maybe UTCTime)
  , _shipCollisions :: ![Number]
} deriving(Show)


data Number = Number {
    _numberPosSpeed :: !PosSpeed
  , _numberNum :: !Int
} deriving(Eq, Show)
