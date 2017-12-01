{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.World.Types
        ( BattleShip(..)
        , World(..)
        , Number(..)
        , BoundedAnimation(..)
        , Boundaries(..)
        , FrameAnimation(..)
        , EmbeddedWorld(..)
        , mkFrameAnimation
        -- ^ reexports
        , module Game.World.Space.Types
        , Terminal.Window(..)
        ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )

import qualified System.Console.Terminal.Size as Terminal( Window(..))

import           Animation.Types

import           Geo.Discrete
import           Game.World.Space.Types

import           Render
import           Timing


data FrameAnimation = FrameAnimation {
    _frameAnimationNextWorld :: !World
  , _frameAnimationStart :: !UTCTime
  , _frameAnimationEase :: !(Float -> Float)
  , _frameAnimationNSteps :: !Int -- in number of frames
  , _frameAnimationProgress :: !Iteration
  , _frameAnimationDeadline :: !(Maybe KeyTime)
}

mkFrameAnimation :: World -- ^ next world
                 -> UTCTime -- ^ time at which the animation starts
                 -> (Float -> Float) -- inverse ease function
                 -> Int -- ^ number of steps
                 -> FrameAnimation
mkFrameAnimation next t ease nsteps =
  FrameAnimation next t ease nsteps (Iteration (Speed 1, startFrame)) (Just $ KeyTime t)
 where
  startFrame = Frame (-1)


data World = World{
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
