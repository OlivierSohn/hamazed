{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.World.Types
        ( BattleShip(..)
        , World(..)
        , Number(..)
        , BoundedAnimation(..)
        , Boundaries(..)
        , FrameAnimation(..)
        , FrameSpec(..)
        , WorldAnimation(..)
        , WorldEvolutions(..)
        , EmbeddedWorld(..)
        , isFinished
        -- | Reexports
        , module Game.World.Space.Types
        , Terminal.Window
        ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )

import qualified System.Console.Terminal.Size as Terminal( Window(..))

import           Animation.Types

import           Geo.Discrete
import           Game.World.Space.Types

import           Interpolation
import           Render
import           Timing

data WorldAnimation = WorldAnimation {
    _worldAnimationFA :: !FrameAnimation
  , _worldAnimationEvs :: !WorldEvolutions
  , _worldAnimationDeadline :: !(Maybe KeyTime)
  , _worldAnimationProgress :: !Iteration
}

data WorldEvolutions = WorldEvolutions {
    _worldEvolutionsUpDown :: !(Evolution [RenderState])
  , _worldEvolutionLeft    :: !(Evolution RenderState)
}

isFinished :: WorldAnimation -> Bool
isFinished (WorldAnimation _ _ Nothing _) = True
isFinished _ = False

-- TODO model frame animation as Evolution / DiscretelyInterpolable
data FrameAnimation = FrameAnimation {
    _frameAnimationNextWorld :: !World
  , _frameAnimationStart :: !(Maybe UTCTime)
  , _frameAnimationDuration :: !Float
  , _frameAnimationInvEase :: !(Float
                             -> Float) -- ^ takes a value, returns a time, both in ranges (0 1)
  , _frameAnimationLastFrame :: !Frame
}

data FrameSpec = FrameSpec {
    _frameSpecSize :: !WorldSize
  , _frameSpecUpperLeft :: !RenderState
} deriving(Eq, Show)

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
  , _embeddedWorldUpperLeft :: !RenderState
}


data BoundedAnimation = BoundedAnimation Animation Boundaries

data Boundaries = WorldFrame
                | TerminalWindow
                | Both


data BattleShip = BattleShip {
    _shipPosSpeed :: !PosSpeed
  , _shipAmmo :: !Int
  , _shipSafeUntil :: !(Maybe UTCTime)
  , _shipCollisions :: ![Number]
}


data Number = Number {
    _numberPosSpeed :: !PosSpeed
  , _numberNum :: !Int
} deriving(Generic, Eq)
