{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.World.Types
        ( World(..)
        , WallDistribution(..)
        , WorldShape(..)
        , BattleShip(..)
        , Number(..)
        , Scope(..)
        , InTerminal(..)
        , ViewMode(..)
        , computeViewDistances
        , environmentInteraction
        -- * Reexports
        , module Imj.Iteration
        , module Imj.Graphics.Text.Animation
        , module Imj.Physics.Discrete.Types
        , Terminal.Window
        , RectContainer(..)
        , RectArea, Filter, Positive
        ) where

import           Imj.Prelude

import qualified System.Console.Terminal.Size as Terminal( Window(..))

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Graphics.Animation.Design.Types
import           Imj.Graphics.Text.Animation
import           Imj.Graphics.UI.RectArea
import           Imj.Graphics.UI.RectContainer
import           Imj.Iteration
import           Imj.Physics.Discrete.Types
import           Imj.Physics.Discrete
import           Imj.Timing


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

data World = World {
    _worldNumbers :: ![Number]
    -- ^ The remaining 'Number's (shot 'Number's are removed from the list)
  , _worldShip :: !BattleShip
    -- ^ The player's 'BattleShip'
  , _worldSpace :: !Space
    -- ^ The 'Space' in which 'BattleShip' and 'Number's evolve
  , _worldAnimations :: ![Animation]
    -- ^ Visual animations. They don't have an influence on the game, they are just here
    -- for aesthetics.
  , _worldEmbedded :: !InTerminal
    -- ^ To know where we should draw the 'World' from, w.r.t terminal frame.
}

data InTerminal = InTerminal {
    _inTerminalSize :: !(Maybe (Terminal.Window Int))
    -- ^ The size of the terminal window
  , _inTerminalShouldCenterShip :: !ViewMode
  , _inTerminalWorldView :: !(RectArea (Filter Positive))
    -- ^ The area of the world view, including outer frame. Its 'Coords' are w.r.t terminal frame.
} deriving (Show)


data ViewMode = CenterShip
              -- ^ the 'BattleShip' position is fixed w.r.t the screen.
              | CenterSpace
              -- ^ the 'Space' frame is fixed w.r.t the screen.
              deriving(Show)

computeViewDistances :: ViewMode -> (Length Width, Length Height)
computeViewDistances CenterShip  = (30, 2) -- it will overlapp for large worlds but that's okay:
                                          -- the overlap is far away from where the ship is
computeViewDistances CenterSpace = (20, 2)

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

-- | An interaction function taking into account a 'World' and 'Scope'
environmentInteraction :: World -> Scope -> Coords Pos -> InteractionResult
environmentInteraction (World _ _ space _ _) scope =
  scopedLocation space scope >>> \case
    InsideWorld  -> Stable
    OutsideWorld -> Mutation
