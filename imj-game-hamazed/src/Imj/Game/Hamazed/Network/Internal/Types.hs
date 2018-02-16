{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Control.Concurrent.MVar(MVar, newEmptyMVar)
import           Control.DeepSeq(NFData(..))
import           Data.Map.Strict(Map, empty)
import           Network.WebSockets(ConnectionException(..), Connection)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types

import           Imj.Geo.Discrete
import           Imj.Game.Hamazed.Loop.Timing

data Client = Client {
    getIdentity :: {-# UNPACK #-} !ClientId
  , getConnection :: {-# UNPACK #-} !Connection
  , getClientType :: {-# UNPACK #-} !ClientType
  , getCurrentWorld :: {-# UNPACK #-} !(Maybe WorldId)
  , getShipSafeUntil :: !(Maybe (Time Point System))
  , getShipAcceleration :: !(Coords Vel)
  -- ^ At the beginning of each level, the ship is immune to collisions with 'Number's
  -- for a given time. This field holds the time at which the immunity ends.
  , getState :: {-# UNPACK #-} !(Maybe PlayerState)
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
    getClients :: {-# UNPACK #-} !Clients
  , _gameTiming :: {-# UNPACK #-} !GameTiming
  , getLevelSpec :: {-# UNPACK #-} !LevelSpec
  , getWorldParameters :: {-# UNPACK #-} !WorldParameters
  -- ^ The actual 'World' is stored on the 'Clients'
  , getLastRequestedWorldId' :: {-# UNPACK #-} !(Maybe WorldId)
  , getIntent' :: {-# UNPACK #-} !Intent
  -- ^ Influences the control flow (how 'ClientEvent's are handled).
  , getShouldTerminate :: {-# UNPACK #-} !Bool
  , getSchedulerSignal :: {-# UNPACK #-} !(MVar WorldId)
  -- ^ When set, it informs the scheduler thread that it should run the game.
} deriving(Generic)
instance NFData ServerState

data Intent =
    Intent'Setup
  | Intent'PlayGame
  | Intent'LevelEnd !LevelOutcome
  deriving(Generic, Show, Eq)
instance NFData Intent

data Clients = Clients {
    getClients' :: !(Map ShipId Client)
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
mkClients = Clients empty (ShipId 0)

newServerState :: IO ServerState
newServerState =
  ServerState mkClients mkGameTiming (mkLevelSpec firstLevel)
              initialParameters Nothing Intent'Setup False <$> newEmptyMVar
