{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.World.Types
        ( World(..)
        , WallDistribution(..)
        , WorldShape(..)
        , BattleShip(..)
        , Number(..)
        , Boundaries(..)
        , mkWorldContainer
        , InTerminal(..)
        , environmentInteraction
        -- * Reexports
        , module Imj.Iteration
        , module Imj.Graphics.Text.Animation
        , module Imj.Physics.Discrete.Types
        , Terminal.Window
        , RectContainer(..)
        ) where

import           Imj.Prelude

import qualified System.Console.Terminal.Size as Terminal( Window(..))

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Geo.Discrete
import           Imj.Graphics.Animation.Design.Types
import           Imj.Graphics.Text.Animation
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

-- | Helper function to create a 'RectContainer' containing a 'World'.
mkWorldContainer :: World -> RectContainer
mkWorldContainer (World _ _ (Space _ sz _) _ (InTerminal _ upperLeft)) =
  RectContainer sz upperLeft

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
  , _inTerminalUpperLeft :: !(Coords Pos)
    -- ^ The 'World' 's 'RectContainer' upper left coordinates,
    -- w.r.t terminal frame.
} deriving (Show)

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

-- | An interaction function taking into account a 'World' and 'Boundaries'
environmentInteraction :: World -> Boundaries -> Coords Pos -> InteractionResult
environmentInteraction (World _ _ space _ (InTerminal mayTermWindow upperLeft)) scope =
  let worldCorner = translate' 1 1 upperLeft
  in scopedLocation space mayTermWindow worldCorner scope >>> \case
    InsideWorld  -> Stable
    OutsideWorld -> Mutation
