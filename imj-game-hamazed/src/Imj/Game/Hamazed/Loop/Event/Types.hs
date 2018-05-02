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
        ) where

import           Imj.Prelude
import           Data.Set(Set)
import           Data.Map(Map)

import           Imj.Game.Hamazed.Level.Types

import           Imj.ClientView.Types
import           Imj.Event
import           Imj.Game.Hamazed.Chat
import           Imj.Log

data HamazedEvent =
     Interrupt !MetaAction
   -- ^ A game interruption.
   | PlayProgram !Int
   deriving(Eq, Show)
instance Categorized HamazedEvent where
  evtCategory = \case
    PlayProgram{}   -> Command'
    Interrupt _ -> Interrupt'

data MetaAction = Help
                -- ^ The player wants to read the help page /(Not implemented yet)/
                deriving(Eq, Show)

data GameStatus =
    New
  | Running
  | Paused !(Set ClientId) !GameStatus
  -- ^ with the list of disconnected clients and status before pause.
  | WaitingForOthersToEndLevel !(Set ClientId)
  | OutcomeValidated !LevelOutcome
  | WhenAllPressedAKey {
      _status :: !GameStatus
    , countdown :: !(Maybe Int)
    , _havePressed :: !(Map ClientId Bool) }
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
