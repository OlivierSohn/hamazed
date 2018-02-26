{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Game.Hamazed.Network.Internal.Types
      ( ServerState(..)
      , Client(..)
      , mkClient
      , PlayerState(..)
      , Clients(..)
      , Intent(..)
      , CurrentGame(..)
      , mkCurrentGame
      , newServerState
      , firstServerLevel
    ) where

import           Imj.Prelude
import           Control.Concurrent.MVar(MVar, newEmptyMVar)
import           Control.DeepSeq(NFData(..))
import           Data.Map.Strict(Map, empty)
import           Data.Set(Set)
import           Network.WebSockets(Connection)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Graphics.Color.Types

import           Imj.Geo.Discrete
import           Imj.Game.Hamazed.Loop.Timing

data Client = Client {
    getName :: {-# UNPACK #-} !PlayerName
  , getConnection :: {-# UNPACK #-} !Connection
  , getServerOwnership :: {-unpack sum-} !ServerOwnership
  , getCurrentWorld :: {-unpack sum-} !(Maybe WorldId)
  , getShipSafeUntil :: {-unpack sum-} !(Maybe (Time Point System))
  -- ^ At the beginning of each level, the ship is immune to collisions with 'Number's
  -- for a given time. This is the time at which the immunity ends. 'Nothing' values
  -- mean that there is no immunity.
  , getShipAcceleration :: !(Coords Vel)
  , getState :: {-unpack sum-} !(Maybe PlayerState) -- TODO should we add Disconnected, and leave disconnected clients in the map?
  -- ^ When 'Nothing', the client is excluded from the current game.
  , getColors :: {-# UNPACK #-} !PlayerColors
} deriving(Generic)
instance NFData Client where
  rnf _ = ()
instance Show Client where
  show (Client a _ c d e f g h) = show (a,c,d,e,f,g,h)

mkClient :: PlayerName -> Color8 Foreground -> Connection -> ServerOwnership -> Client
mkClient a color b c =
  Client a b c Nothing Nothing zeroCoords Nothing $ mkPlayerColors color

data PlayerState = InGame | Finished
  deriving (Generic, Eq, Show)
instance NFData PlayerState

-- | A 'Server' handles one game only (for now).
data ServerState = ServerState {
    serverLogs :: {-unpack sum-} !ServerLogs
  , getClients :: {-# UNPACK #-} !Clients
  , gameTiming :: !GameTiming -- could / should this be part of CurrentGame?
  , levelSpecification :: {-# UNPACK #-} !LevelSpec
  , worldParameters :: {-# UNPACK #-} !WorldParameters
  -- ^ The actual 'World' is stored on the 'Clients'
  , lastRequestedWorldId :: {-unpack sum-} !(Maybe WorldId)
  , intent :: {-unpack sum-} !Intent
  -- ^ Influences the control flow (how 'ClientEvent's are handled).
  , startSecond :: {-# UNPACK #-} !Int
  -- ^ The second at which the 'ServerState' was created
  , shouldTerminate :: {-unpack sum-} !Bool
  -- ^ Set on server shutdown
  , scheduledGame :: {-# UNPACK #-} !(MVar CurrentGame)
  -- ^ When set, it informs the scheduler thread that it should run the game.
} deriving(Generic)
instance NFData ServerState

data CurrentGame = CurrentGame {
    getGameWorld :: {-# UNPACK #-} !WorldId
  , getGamePlayers :: !(Set ShipId)
  , getGameStatus :: {-unpack sum-} !GameStatus
} deriving(Generic, Show)

mkCurrentGame :: WorldId -> Set ShipId -> CurrentGame
mkCurrentGame w s = CurrentGame w s New

data Intent =
    IntentSetup
  | IntentPlayGame
  | IntentLevelEnd !LevelOutcome
  deriving(Generic, Show, Eq)
instance NFData Intent

data Clients = Clients {
    getClients' :: !(Map ShipId Client)
    -- once a client is disconnected, it is removed from the map.
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

firstServerLevel :: Int
firstServerLevel = firstLevel

newServerState :: ServerLogs -> IO ServerState
newServerState logs = do
  t <- getCurrentSecond
  ServerState logs mkClients mkGameTiming (LevelSpec firstServerLevel CannotOvershoot)
              initialParameters Nothing IntentSetup t False <$> newEmptyMVar
