{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Game.Status
        ( GameStatus(..)
        , StateNature(..)
        , ClientState(..)
        , GameStateValue(..)
        ) where

import           Imj.Prelude
import           Data.Set(Set)
import           Data.Map(Map)

import           Imj.ClientView.Types
import           Imj.Game.Level
import           Imj.Server.Class

data ClientState s = ClientState {-unpack sum-} !StateNature {-unpack sum-} !(StateValue s)
  deriving(Generic, Show, Eq)

data StateNature = Ongoing | Over
  deriving(Generic, Show, Eq)
instance Binary StateNature

data GameStateValue =
    Setup
  -- ^ The player is configuring the game
  | PlayLevel !GameStatus
  -- ^ The player is playing the game
  deriving(Generic, Show, Eq)
instance Binary GameStateValue
instance NFData GameStateValue

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
