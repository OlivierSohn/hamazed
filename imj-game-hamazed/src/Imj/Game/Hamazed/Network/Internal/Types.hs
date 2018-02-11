{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Hamazed.Network.Internal.Types
      ( ServerState(..)
      , Client(..)
      , Clients(..)
      , Intent(..)
      , mkClient
      , newServerState
    ) where

import           Imj.Prelude hiding(intercalate)
import           Control.DeepSeq(NFData(..))
import           Network.WebSockets(Connection)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types

import           Imj.Game.Hamazed.Loop.Timing

-- | A 'Server' handles one game only (for now).
data ServerState = ServerState {
    getClients :: !Clients
  , getPlayingClients :: ![(Client, Maybe WorldId)]
  , _gameTiming :: !GameTiming
  , getLevelSpec :: !LevelSpec
  , getWorldParameters :: !WorldParameters
  -- ^ The actual 'World' is stored on the 'Clients'
  , getLastRequestedWorldId :: !(Maybe WorldId)
  , getIntent :: !Intent
} deriving(Generic)
instance NFData ServerState

data Intent =
    Intent'Setup
  | Intent'PlayGame
  | Intent'GameEnd !GameOutcome
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

newServerState :: ServerState
newServerState =
  ServerState mkClients [] mkGameTiming (mkLevelSpec firstLevel)
              initialParameters Nothing Intent'Setup

data Client = Client {
    getIdentity :: !ClientId
  , shipSafeUntil :: !(Maybe (Time Point System))
  -- ^ At the beginning of each level, the ship is immune to collisions with 'Number's
  -- for a given time. This field holds the time at which the immunity ends.
  , getConnection :: !Connection
  , getClientType :: !ClientType
} deriving(Generic)
instance NFData Client where
  rnf _ = ()

mkClient :: Connection -> ClientType -> ClientId -> Client
mkClient c t i = Client i Nothing c t
