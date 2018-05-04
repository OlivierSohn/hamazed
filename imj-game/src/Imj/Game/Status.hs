{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Game.Status
        ( GameStatus(..)
        , StateValue(..)
        , PlayerStatus(..) -- TODO should we merge with 'StateValue' ?
        , StateNature(..)
        , ClientState(..)
        ) where

import           Imj.Prelude
import           Data.Set(Set)
import           Data.Map(Map)

import           Imj.ClientView.Types
import           Imj.Game.Level

data PlayerStatus = Present | Absent
  deriving(Generic, Show)
instance Binary PlayerStatus

data ClientState = ClientState {-unpack sum-} !StateNature {-unpack sum-} !StateValue
  deriving(Generic, Show, Eq)

data StateNature = Ongoing | Over
  deriving(Generic, Show, Eq)
instance Binary StateNature

data StateValue =
    Excluded
    -- ^ The player is not part of the game
  | Setup
  -- ^ The player is configuring the game
  | PlayLevel !GameStatus
  -- ^ The player is playing the game
  deriving(Generic, Show, Eq)
instance Binary StateValue
instance NFData StateValue

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
