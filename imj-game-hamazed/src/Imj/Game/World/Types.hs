{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.World.Types
        ( World(..)
        , WallDistribution(..)
        , WorldShape(..)
        , BattleShip(..)
        , Number(..)
        , BoundedAnimation(..)
        , Boundaries(..)
        , mkFrameSpec
        , UIAnimation(..)
        , UIEvolutions(..)
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


data WorldShape = Square
                -- ^ Width = Height
                | Rectangle2x1
                -- ^ Width = 2 * Height

-- | How should walls be created?
data WallDistribution = None
              -- ^ No 'Wall's.
              | Deterministic
              -- ^ A Rectangular 'Wall' in the middle of the level.
              | Random !RandomParameters
              -- ^ 'Wall's are created with an algorithm involving random numbers.

-- | Manages the progress and deadline of 'UIEvolutions'.
data UIAnimation = UIAnimation {
    _uiAnimationEvs :: !UIEvolutions
  , _uiAnimationDeadline :: !(Maybe KeyTime)
  -- ^ Time at which the 'UIEvolutions' should be rendered and updated
  , _uiAnimationProgress :: !Iteration
  -- ^ Current 'Iteration'.
} deriving(Show)

-- | Used when transitionning between two levels to smoothly transform the aspect
-- of the 'RectFrame', as well as textual information around it.
data UIEvolutions = UIEvolutions {
    _uiEvolutionFrame :: !(Evolution RectFrame)
    -- ^ The transformation of the 'RectFrame'.
  , _uiEvolutionsUpDown :: !(TextAnimation AnchorChars)
    -- ^ The transformation of colored text at the top and at the bottom of the 'RectFrame'.
  , _uiEvolutionLeft    :: !(TextAnimation AnchorStrings)
    -- ^ The transformation of colored text left and right of the 'RectFrame'.
} deriving(Show)

-- | Is the 'UIAnimation' finished?
isFinished :: UIAnimation -> Bool
isFinished (UIAnimation _ Nothing _) = True
isFinished _ = False

-- | Helper function to create a 'RectFrame' placed around the 'World' limits.
mkFrameSpec :: LayeredColor -> World -> RectFrame
mkFrameSpec colors (World _ _ (Space _ sz _) _ (EmbeddedWorld _ upperLeft)) =
  RectFrame sz upperLeft colors

data World = World {
    _worldNumbers :: ![Number]
    -- ^ The remaining 'Number's (shot 'Number's are removed from the list)
  , _worldShip :: !BattleShip
    -- ^ The player's 'BattleShip'
  , _worldSpace :: !Space
    -- ^ The 'Space' in which 'BattleShip' and 'Number's evolve
  , _worldAnimations :: ![BoundedAnimation]
    -- ^ Visual animations. They don't have an influence on the game, they are just here
    -- for aesthetics.
  , _worldEmbedded :: !EmbeddedWorld
    -- ^ To know where we should draw the 'World' from, w.r.t terminal frame.
}

data EmbeddedWorld = EmbeddedWorld {
    _embeddedWorldTerminal :: !(Maybe (Terminal.Window Int))
    -- ^ The size of the terminal window
  , _embeddedWorldUpperLeft :: !Coords
    -- ^ The corresponding 'World' upper left coordinates w.r.t terminal frame.
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
  -- ^ Which number it represents (1 to 16).
} deriving(Eq, Show)
