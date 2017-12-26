{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.World.Types
        (
          World(..)
        , WallDistribution(..)
        , BattleShip(..)
        , Number(..)
        , BoundedAnimation(..)
        , Boundaries(..)
        , mkFrameSpec
        , WorldAnimation(..)
        , WorldEvolutions(..)
        , EmbeddedWorld(..)
        , isFinished
        -- * Reexports
        , module Imj.Iteration
        , module Imj.Text.Animation
        , module Imj.Physics.Discrete.Types
        , Terminal.Window
        , RectFrame(..)
        ) where

import           Imj.Prelude

import qualified System.Console.Terminal.Size as Terminal( Window(..))

import           Imj.Animation.Design.Types
import           Imj.Game.World.Space.Types
import           Imj.Iteration
import           Imj.Physics.Discrete.Types
import           Imj.Text.Animation
import           Imj.Text.ColorString
import           Imj.Timing
import           Imj.UI.RectFrame


-- | How should walls be created?
data WallDistribution = None
              -- ^ No 'Wall's.
              | Deterministic
              -- ^ A Rectangular 'Wall' in the middle of the level.
              | Random !RandomParameters
              -- ^ 'Wall's are created with an algorithm involving random numbers.

-- | Manages the progress and deadline of the 'WorldEvolutions' animation.
data WorldAnimation = WorldAnimation {
    _worldAnimationEvs :: !WorldEvolutions
  , _worldAnimationDeadline :: !(Maybe KeyTime)
  -- ^ Time at which the 'WorldEvolution' should be rendered and updated
  , _worldAnimationProgress :: !Iteration
  -- ^ Current 'Iteration' of the animation.
} deriving(Show)

-- | Used when transitionning between two levels to smoothly transform the aspect
-- of the 'RectFrame' around the world, as well as textual information around it.
data WorldEvolutions = WorldEvolutions {
    _worldEvolutionFrame :: !(Evolution RectFrame)
    -- ^ The transformation of the 'RectFrame' around the 'World'.
  , _worldEvolutionsUpDown :: !(TextAnimation AnchorChars)
    -- ^ The transformation of colored text at the top and at the bottom of the 'RectFrame'.
  , _worldEvolutionLeft    :: !(TextAnimation AnchorStrings)
    -- ^ The transformation of colored text left and right of the 'RectFrame'.
} deriving(Show)

-- | Is the 'WorldAnimation' finished?
isFinished :: WorldAnimation -> Bool
isFinished (WorldAnimation _ Nothing _) = True
isFinished _ = False

-- | Helper function to create a 'RectFrame' placed around the 'World' limits.
mkFrameSpec :: LayeredColor -> World -> RectFrame
mkFrameSpec colors (World _ _ _ (Space _ sz _) _ (EmbeddedWorld _ upperLeft)) =
  RectFrame sz upperLeft colors

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
} deriving (Show)

data BoundedAnimation = BoundedAnimation !Animation !Boundaries deriving(Show)

data BattleShip = BattleShip {
    _shipPosSpeed :: !PosSpeed
  -- ^ Discrete position and speed.
  , _shipAmmo :: !Int
  -- ^ How many laser shots are left.
  , _shipSafeUntil :: !(Maybe SystemTime)
  -- ^ At the beginning of each level, the ship is immune to collisions with 'Number's
  -- for a given time. This field holds the time at which the immunity ends.
  , _shipCollisions :: ![Number]
  -- ^ Which 'Number's are currently colliding with the 'BattleShip'.
} deriving(Show)


data Number = Number {
    _numberPosSpeed :: !PosSpeed
  -- ^ Discrete position and speed.
  , _numberNum :: !Int
  -- ^ Which number it represents.
} deriving(Eq, Show)
