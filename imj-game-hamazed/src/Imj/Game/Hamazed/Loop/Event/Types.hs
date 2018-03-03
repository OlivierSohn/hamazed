{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Game.Hamazed.Loop.Event.Types
        ( Event(..)
        , MessageLevel(..)
        , ChatCommand(..)
        , Deadline(..)
        , ActionTarget(..)
        , DeadlineType(..)
        , MetaAction(..)
        , ShipId(..)
        -- * Reexports (for haddock hyperlinks)
        , module Imj.Game.Hamazed.World.Types
        , module Imj.Graphics.ParticleSystem.Design.Create
        ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.World.Types
import           Imj.Game.Hamazed.Level.Types

import           Imj.Game.Hamazed.Chat
import           Imj.Graphics.ParticleSystem.Design.Create
import           Imj.Log
import           Imj.Timing

-- | A foreseen game or animation update.
data Deadline = Deadline {
    _deadlineTime :: {-# UNPACK #-} !(Time Point System)
    -- ^ At which time should the update become visible to the user.
  , _deadlinePriority :: {-# UNPACK #-} !Int
  , _deadlineType :: {-unpack sum-} !DeadlineType
} deriving(Eq, Show)


data DeadlineType = AnimateParticleSystem {-# UNPACK #-} !ParticleSystemKey
                  -- ^ Update one or more 'ParticleSystem's.
                  | DisplayContinueMessage
                  -- ^ Show the /Hit a key to continue/ message
                  | AnimateUI
                  -- ^ Update the inter-level animation
                  deriving(Eq, Show)

data Event = Configuration !Char
           -- ^ Configures game parameters
           | CycleRenderingOptions
           -- ^ Changes the font used to render
           | Timeout !Deadline
           -- ^ The 'Deadline' that needs to be handled immediately.
           | EndLevel !LevelOutcome
           -- ^ End of level.
           | Interrupt !MetaAction
           -- ^ A game interruption.
           | ToggleEventRecording
           | Log !MessageLevel !Text
           | ChatCmd {-unpack sum-} !ChatCommand
           | SendChatMessage
           -- ^ Send message or execute command if the message starts with a '/'
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
                  deriving(Generic, Eq, Show, Binary)
