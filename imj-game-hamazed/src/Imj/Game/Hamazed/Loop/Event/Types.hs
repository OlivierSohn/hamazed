{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Game.Hamazed.Loop.Event.Types
        ( Event(..)
        , MessageLevel(..)
        , ChatCommand(..)
        , GameStatus(..)
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
import           Control.DeepSeq(NFData(..))

import           Imj.Game.Hamazed.World.Types
import           Imj.Game.Hamazed.Level.Types
import           Imj.Geo.Discrete.Types

import           Imj.Game.Hamazed.Chat
import           Imj.Graphics.Font
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
                  | AnimateUI
                  -- ^ Update the inter-level animation
                  | RedrawStatus {-# UNPACK #-} !(Frame,Int)
                  -- ^ The status is being progressively displayed, with line index.
                  deriving(Eq, Show)

data Event = CycleRenderingOptions {-# UNPACK #-} !CycleFont {-# UNPACK #-} !CycleFontSize
           -- ^ Changes the font used to render
           | ApplyPPUDelta {-# UNPACK #-} !Size
           | ApplyFontMarginDelta {-# UNPACK #-} !FontMargin
           | CanvasSizeChanged
            -- ^ Produced by the delta renderer when a size change was detected upon rendering.
           | RenderingTargetChanged
            -- ^ Produced by the platform
           | Timeout {-# UNPACK #-} !Deadline
           -- ^ The 'Deadline' that needs to be handled immediately.
           | Interrupt !MetaAction
           -- ^ A game interruption.
           | ToggleEventRecording
           | Log !MessageLevel !Text
           | ChatCmd {-unpack sum-} !ChatCommand
           | SendChatMessage
           -- ^ Send message or execute command if the message starts with a '/'
           deriving(Eq, Show)

data MetaAction = Help
                -- ^ The player wants to read the help page /(Not implemented yet)/
                deriving(Eq, Show)

data GameStatus =
    New
  | Running
  | Paused !(Set ShipId) !GameStatus
  -- ^ with the list of disconnected clients and status before pause.
  | WaitingForOthersToEndLevel !(Set ShipId)
  | OutcomeValidated !LevelOutcome
  | WhenAllPressedAKey {
      _status :: !GameStatus
    , countdown :: !(Maybe Int)
    , _havePressed :: !(Map ShipId Bool) }
  -- ^ Maybe Int is a countdown : when Nothing, the message is displayed.
  -- A 'True' in the 'Map' means the key was pressed.
  | Countdown !Int !GameStatus
  -- ^ Int is in seconds
  | CancelledNoConnectedPlayer
  deriving(Generic, Show, Eq)
instance Binary GameStatus
instance NFData GameStatus

data ActionTarget = Ship
                  -- ^ The player wants to accelerate the 'BattleShip'
                  | Laser
                  -- ^ The player wants to shoot with the laser.
                  deriving(Generic, Eq, Show, Binary)
