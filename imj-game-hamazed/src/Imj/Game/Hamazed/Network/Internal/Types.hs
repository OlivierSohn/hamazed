{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Hamazed.Network.Internal.Types
      ( ServerState(..)
      , Client(..)
      , mkClient
      , PlayerState(..)
      , Clients(..)
      , Intent(..)
      , newServerState
    ) where

import           Imj.Prelude hiding(intercalate)
import           Control.DeepSeq(NFData(..))
import           Network.WebSockets(Connection)
import           Control.Concurrent.MVar(MVar, newEmptyMVar)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types

import           Imj.Geo.Discrete
import           Imj.Game.Hamazed.Loop.Timing

data Client = Client {
    getIdentity :: !ClientId
  , getConnection :: !Connection
  , getClientType :: !ClientType
  , getCurrentWorld :: !(Maybe WorldId)
  , getShipSafeUntil :: !(Maybe (Time Point System))
  , getShipAcceleration :: !(Coords Vel)
  -- ^ At the beginning of each level, the ship is immune to collisions with 'Number's
  -- for a given time. This field holds the time at which the immunity ends.
  , getState :: !(Maybe PlayerState)
  -- ^ When 'Nothing', the client is excluded from the current game.
} deriving(Generic)
instance NFData Client where
  rnf _ = ()

mkClient :: ClientId -> Connection -> ClientType -> Client
mkClient a b c = Client a b c Nothing Nothing zeroCoords Nothing

data PlayerState = InGame | Finished
  deriving (Generic, Eq)
instance NFData PlayerState

-- | A 'Server' handles one game only (for now).
data ServerState = ServerState {
    getClients :: !Clients
  , _gameTiming :: !GameTiming
  , getLevelSpec :: !LevelSpec
  , getWorldParameters :: !WorldParameters
  -- ^ The actual 'World' is stored on the 'Clients'
  , getLastRequestedWorldId' :: !(Maybe WorldId)
  , getIntent' :: !Intent
  , getSchedulerSignal :: !(MVar WorldId)
} deriving(Generic)
instance NFData ServerState

data Intent =
    Intent'Setup
  | Intent'PlayGame
  | Intent'LevelEnd !LevelOutcome
  deriving(Generic, Show, Eq)
instance NFData Intent

data Clients = Clients {
    getClients' :: ![Client]
  , getNextShipId :: !ShipId
} deriving(Generic)
instance NFData Clients

data GameTiming = GameTiming {
    _gameStateNextMotionStep :: !(Maybe (Time Point System))
  -- ^ When the next 'World' motion update should happen
  , _gameStateTimeMultiplicator :: !(Multiplicator GameTime)
} deriving(Generic)
instance NFData GameTiming

mkGameTiming :: GameTiming
mkGameTiming = GameTiming Nothing initalGameMultiplicator

mkClients :: Clients
mkClients = Clients [] (ShipId 0)

newServerState :: IO ServerState
newServerState =
  ServerState mkClients mkGameTiming (mkLevelSpec firstLevel)
              initialParameters Nothing Intent'Setup <$> newEmptyMVar
