{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.World.Types
        ( BattleShip(..)
        , World(..)
        , Number(..)
        , BoundedAnimation(..)
        , Boundaries(..)
        , FrameSpec(..)
        , TextAnimSpec(..)
        , mkFrameSpec
        , WorldAnimation(..)
        , WorldEvolutions(..)
        , EmbeddedWorld(..)
        , isFinished
        -- | Reexports
        , module Game.World.Space.Types
        , module Game.World.Frame.Types
        , module Iteration
        , module Text.Animated
        , Terminal.Window
        ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )
import           GHC.Show(showString)

import qualified System.Console.Terminal.Size as Terminal( Window(..))

import           Animation.Types

import           Geo.Discrete

import           Game.World.Space.Types
import           Game.World.Frame.Types

import           Iteration

import           Render

import           Text.Animated

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

mkFrameSpec :: LayeredColor -> World -> IORef Buffers -> FrameSpec
mkFrameSpec colors (World _ _ _ (Space _ sz _) _ (EmbeddedWorld _ upperLeft _)) =
  FrameSpec sz upperLeft colors

data World = World {
    _worldNumbers :: ![Number]
  , _howBallMoves :: Space -> PosSpeed -> PosSpeed
  , _worldShip :: !BattleShip
  , _worldSpace :: !Space
  , _worldAnimations :: ![BoundedAnimation]
  , _worldEmbedded :: !EmbeddedWorld
}

data EmbeddedWorld = EmbeddedWorld {
    _embeddedWorldTerminal :: !(Maybe (Terminal.Window Int))
  , _embeddedWorldUpperLeft :: !Coords
  , _embeddedWorldRenderBuffers :: IORef Buffers
}

instance Show EmbeddedWorld where
  showsPrec _ (EmbeddedWorld a b _) = showString $ show (a,b,"IORef")


data BoundedAnimation = BoundedAnimation Animation Boundaries deriving(Show)

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
} deriving(Generic, Eq, Show)
