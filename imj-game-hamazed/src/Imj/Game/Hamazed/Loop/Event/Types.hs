{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Loop.Event.Types
        ( Event(..)
        , Deadline(..)
        , ActionTarget(..)
        , DeadlineType(..)
        , MetaAction(..)
        -- * Reexports (for haddock hyperlinks)
        , module Imj.Game.Hamazed.World.Types
        , module Imj.Graphics.ParticleSystem.Design.Create
        ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Types
import           Imj.Geo.Discrete
import           Imj.Graphics.ParticleSystem.Design.Create
import           Imj.Timing

-- | A foreseen game or animation update.
data Deadline = Deadline {
    _deadlineTime :: !KeyTime
    -- ^ At which time should the update become visible to the user.
  , _deadlinePriority :: !Int
  , _deadlineType :: !DeadlineType
} deriving(Eq, Show)

data DeadlineType = MoveFlyingItems
                  -- ^ Move 'Number's and 'BattleShip' according to their current
                  -- speeds.
                  | AnimateParticleSystems
                  -- ^ Update one or more 'ParticleSystem's.
                  | DisplayContinueMessage
                  -- ^ Show the /Hit a key to continue/ message
                  | AnimateUI
                  -- ^ Update the inter-level animation
                  deriving(Eq, Show)

data Event = Configuration !Char
           -- ^ Configures game parameters
           | StartGame
           -- ^ To transition from configuration mode to play mode.
           | Action !ActionTarget !Direction
           -- ^ A player action on an 'ActionTarget' in a 'Direction'.
           | Timeout !Deadline
           -- ^ The 'Deadline' that needs to be handled immediately.
           | StartLevel !Int
           -- ^ New level.
           | EndGame
           -- ^ End of game.
           | Interrupt !MetaAction
           -- ^ A game interruption.
           | ToggleEventRecording
           deriving(Eq, Show)

data MetaAction = Quit
                -- ^ The player decided to quit the game.
                | Help
                -- ^ The player wants to read the help page /(Not implemented yet)/
                deriving(Eq, Show)

data ActionTarget = Ship
                  -- ^ The player wants to accelerate the 'BattleShip'
                  | Laser
                  -- ^ The player wants to shoot with the laser.
                  deriving(Eq, Show)
