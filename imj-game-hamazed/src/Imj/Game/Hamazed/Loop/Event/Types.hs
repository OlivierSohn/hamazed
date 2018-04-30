{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Loop.Event.Types
        ( MessageLevel(..)
        , ChatCommand(..)
        , GameStatus(..)
        , ActionTarget(..)
        , MetaAction(..)
        , HamazedEvent(..)
        -- * Reexports (for haddock hyperlinks)
        , module Imj.Event -- TODO remove
        , module Imj.Game.Hamazed.World.Types
        , module Imj.Graphics.ParticleSystem.Design.Create
        ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.World.Types
import           Imj.Game.Hamazed.Level.Types

import           Imj.Event
import           Imj.Game.Hamazed.Chat
import           Imj.Graphics.ParticleSystem.Design.Create
import           Imj.Log

data HamazedEvent =
     Interrupt !MetaAction
   -- ^ A game interruption.
   | ChatCmd {-unpack sum-} !ChatCommand
   | SendChatMessage
   -- ^ Send message or execute command if the message starts with a '/'
   | PlayProgram !Int
   deriving(Eq, Show)
instance Categorized HamazedEvent where
  evtCategory = \case
    PlayProgram{}   -> Command'
    SendChatMessage -> Command'
    ChatCmd _       -> Command'
    Interrupt _ -> Interrupt'

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
