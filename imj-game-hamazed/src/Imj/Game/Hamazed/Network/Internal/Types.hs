{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Game.Hamazed.Network.Internal.Types
      ( WorldState(..)
      , WorldCreation(..)
      , HamazedClient(..)
      , HamazedClientEvent(..)
      , HamazedServerEvent(..)
      , PlayerState(..)
      , Intent(..)
      , CurrentGame(..)
      , mkCurrentGame
      , firstServerLevel
      , GameTiming(..)
      , WorldRequestArg(..)
      , GameNotif(..)
      , GameStep(..)
      , GameStatus(..)
      , HamazedEnumValueKey(..)
      , HamazedValue(..)
      , HamazedValueKey(..)
      -- * Game
      , GameStateEssence(..)
      , ShotNumber(..)
      , Operation(..)
      -- * Scheduler
      , RunResult(..)
    ) where

import           Imj.Prelude
import           Data.List(unwords)
import qualified Data.Set as Set

import           Imj.Categorized
import           Imj.Game.Hamazed.World.Types
import           Imj.Game.Hamazed.Level
import           Imj.Server.Class
import           Imj.Space.Types

import           Imj.Audio
import           Imj.Game.Class
import           Imj.Game.Level
import           Imj.Game.Status
import           Imj.Game.HighScores
import           Imj.Game.Hamazed.Timing
import           Imj.Music.Instruments
import           Imj.Timing

-- | An event generated by the client, sent to the server.
data HamazedClientEvent =
    WorldProposal !WorldId !(MkSpaceResult WorldEssence) !(Map Properties Statistics)
    -- ^ In response to 'WorldRequest' 'Build'
  | CurrentGameState {-# UNPACK #-} !WorldId !(Maybe GameStateEssence)
    -- ^ In response to 'WorldRequest' 'GetGameState'
  | IsReady {-# UNPACK #-} !WorldId
  -- ^ When the level's UI transition is finished.
  | Action {-unpack sum-} !ActionTarget {-unpack sum-} !Direction
   -- ^ A player action on an 'ActionTarget' in a 'Direction'.
  | LevelEnded {-unpack sum-} !LevelOutcome
  | CanContinue {-unpack sum-} !GameStatus
  deriving(Generic, Show)
instance Binary HamazedClientEvent

-- | An event generated by the server, sent to a client.
data HamazedServerEvent =
    GameInfo {-unpack sum-} !GameNotif
  | WorldRequest {-# UNPACK #-} !WorldId
                                !WorldRequestArg
  | ChangeLevel {-# UNPACK #-} !LevelEssence -- TODO merge with WorldRequest
                {-# UNPACK #-} !WorldEssence
                {-# UNPACK #-} !WorldId
  -- ^ Triggers a UI transition between the previous (if any) and the next level.
  | PutGameState {-# UNPACK #-} !GameStateEssence  -- TODO merge with WorldRequest
                 {-# UNPACK #-} !WorldId
  -- ^ (reconnection scenario) Upon reception, the client should set its gamestate accordingly.
  | GameEvent {-unpack sum-} !GameStep
  deriving(Generic, Show)
instance Binary HamazedServerEvent
instance DrawGroupMember HamazedServerEvent where
  exclusivityKeys = \case
    GameEvent LaserShot{} -> Set.singleton GameStep
    GameEvent PeriodicMotion{} -> Set.singleton GameStep
    WorldRequest{} -> mempty
    ChangeLevel{} -> mempty
    PutGameState{} -> mempty
    GameInfo _ -> mempty
instance Categorized HamazedServerEvent where
  evtCategory = \case
    GameEvent LaserShot{} -> Laser'
    GameEvent PeriodicMotion{} -> PeriodicMotion'
    WorldRequest{} -> WorldRequest'
    ChangeLevel{} -> ChangeLevel'
    PutGameState{} -> ChangeLevel'
    GameInfo _ -> Chat'

data WorldRequestArg =
    Build {-# UNPACK #-} !(Time Duration System)
          {-# UNPACK #-} !WorldSpec
  | Cancel
  | GetGameState
  -- ^ Upon 'Build' reception, the client should respond with a 'WorldProposal', within the
  -- given duration, except if a later 'Cancel' for the same 'WorldId' is received.
  --
  -- Upon 'GetGameState' reception, the client responds with a 'CurrentGameState'
  deriving(Generic, Show)
instance Binary WorldRequestArg

data GameNotif =
    LevelResult {-# UNPACK #-} !LevelNumber {-unpack sum-} !LevelOutcome
  | GameWon
  | Highscores !HighScores
  | CannotCreateLevel ![Text] {-# UNPACK #-} !LevelNumber
  deriving(Generic, Show)
instance Binary GameNotif

-- | Identifiers of values shared by all players.
data HamazedEnumValueKey = -- This could be an instance of WorldParameters
    BlockSize
  | WallProbability
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary HamazedEnumValueKey
instance NFData HamazedEnumValueKey

-- | ServerContent shared by all players.
data HamazedValue =
    WorldShape {-unpack sum-} !WorldShape
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary HamazedValue
instance NFData HamazedValue
instance ChatShow HamazedValue where
  chatShow (WorldShape shape) =
    unwords ["world shape:", show shape]

-- | Identifiers of values shared by all players.
data HamazedValueKey =
    WorldShapeKey
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary HamazedValueKey
instance NFData HamazedValueKey

-- | 'PeriodicMotion' aggregates the accelerations of all ships during a game period.
data GameStep =
  PeriodicMotion {
    _shipsAccelerations :: !(Map ClientId (Coords Vel))
    , _shipsLostArmor :: !(Set ClientId)
  }
  | LaserShot {-unpack sum-} !Direction {-# UNPACK #-} !ClientId
  deriving(Generic, Show)
instance Binary GameStep

data GameStateEssence = GameStateEssence {
    _essence :: {-# UNPACK #-} !WorldEssence
  , _shotNumbers :: ![ShotNumber]
  , _levelEssence :: {-unpack sum-} !LevelEssence
} deriving(Generic, Show)
instance Binary GameStateEssence

data ShotNumber = ShotNumber {
    getNumberValue :: {-# UNPACK #-} !Int
    -- ^ The numeric value
  , getOperation :: !Operation
  -- ^ How this number influences the current sum.
} deriving (Generic, Show)
instance Binary ShotNumber

data Operation = Add | Substract
  deriving (Generic, Show)
instance Binary Operation

data HamazedClient = HamazedClient {
    getCurrentWorld :: {-unpack sum-} !(Maybe WorldId)
  , getShipSafeUntil :: {-unpack sum-} !(Maybe (Time Point System))
  -- ^ At the beginning of each level, the ship is immune to collisions with 'Number's
  -- for a given time. This is the time at which the immunity ends. 'Nothing' values
  -- mean that there is no immunity.
  , getShipAcceleration :: !(Coords Vel)
  , getState :: {-unpack sum-} !(Maybe PlayerState) -- TODO should we add Disconnected, and leave disconnected clients in the map?
  -- ^ When 'Nothing', the client is excluded from the current game.
} deriving(Generic, Show)
instance NFData HamazedClient


data PlayerState =
    Playing {-unpack sum-} !(Maybe LevelOutcome)
  | ReadyToPlay
  deriving (Generic, Eq, Show)
instance NFData PlayerState

data WorldCreation = WorldCreation {
    creationState :: !WorldState
  , creationKey :: !WorldId
  , creationSpec :: !WorldSpec
  , creationStatistics :: !(Map Properties Statistics)
  -- ^ Statistics stop being gathered once the world is created
} deriving(Generic)
instance NFData WorldCreation

data WorldState =
    CreationAssigned !(Set ClientId) -- which clients are responsible for creating the world
  | Created
  deriving(Generic, Show)
instance NFData WorldState

data CurrentGame = CurrentGame {
    gameWorld :: {-# UNPACK #-} !WorldId
  , gamePlayers' :: !(Set ClientId)
  , status' :: {-unpack sum-} !GameStatus
  , score :: !(Score InstrumentId)
} deriving(Generic, Show)

mkCurrentGame :: Score InstrumentId -> WorldId -> Set ClientId -> CurrentGame
mkCurrentGame sc w s = CurrentGame w s New sc

data Intent =
    IntentSetup
  | IntentPlayGame !(Maybe LevelOutcome)
  deriving(Generic, Show, Eq)
instance NFData Intent

data GameTiming = GameTiming {
    _gameStateNextMotionStep :: !(Maybe (Time Point System))
  -- ^ When the next 'World' motion update should happen
  , _gameStateTimeMultiplicator :: !(Multiplicator GameTime)
} deriving(Generic)
instance NFData GameTiming

firstServerLevel :: LevelNumber
firstServerLevel = firstLevel

data RunResult =
    NotExecutedGameCanceled
  | NotExecutedTryAgainLater !(Time Duration System)
  -- ^ withe the duration to sleep before retrying
  | Executed !(Maybe (Time Duration System))
  -- ^ With an optional duration to wait before the next iteration
