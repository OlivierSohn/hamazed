{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Network.Internal.Types
      ( ServerState(..)
      , WorldState(..)
      , WorldCreation(..)
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
import           Control.Concurrent.MVar.Strict(MVar, newEmptyMVar)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map (empty)
import           Data.Set(Set)
import qualified Data.Set as Set (empty)
import           Network.WebSockets(Connection)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Color.Types

import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.Color

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
  , getColor :: {-# UNPACK #-} !(Color8 Foreground)
  -- ^ Ship color, deduced from the 'centerColor' of the 'ServerState'
} deriving(Generic)
instance NFData Client where
  rnf _ = ()
instance Show Client where
  show (Client a _ c d e f g h) = show (a,c,d,e,f,g,h)

mkClient :: PlayerName -> Color8 Foreground -> Connection -> ServerOwnership -> Client
mkClient a color b c =
  Client a b c Nothing Nothing zeroCoords Nothing color

data PlayerState =
    Playing {-unpack sum-} !(Maybe LevelOutcome)
  | ReadyToPlay
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
  , worldCreation :: {-unpack sum-} !WorldCreation
  , intent :: {-unpack sum-} !Intent
  -- ^ Influences the control flow (how 'ClientEvent's are handled).
  , centerColor :: {-# UNPACK #-} !(Color8 Foreground)
  -- ^ The color scheme.
  , shouldTerminate :: {-unpack sum-} !Bool
  -- ^ Set on server shutdown
  , scheduledGame :: {-# UNPACK #-} !(MVar CurrentGame)
  -- ^ When set, it informs the scheduler thread that it should run the game.
} deriving(Generic)
instance NFData ServerState

data WorldCreation = WorldCreation {
    creationState :: !WorldState
  , creationKey :: !WorldId
  , creationSpec :: !WorldSpec
  , creationStatistics :: !(Map Properties Statistics)
  -- ^ Statistics stop being gathered once the world is created
} deriving(Generic)
instance NFData WorldCreation

mkWorldCreation :: WorldSpec -> WorldCreation
mkWorldCreation spec = WorldCreation (CreationAssigned Set.empty) (WorldId 0) spec Map.empty

data WorldState =
    CreationAssigned !(Set ShipId) -- which clients are responsible for creating the world
  | Created
  deriving(Generic, Show)
instance NFData WorldState

data CurrentGame = CurrentGame {
    gameWorld :: {-# UNPACK #-} !WorldId
  , gamePlayers' :: !(Set ShipId)
  , status' :: {-unpack sum-} !GameStatus
} deriving(Generic, Show)

mkCurrentGame :: WorldId -> Set ShipId -> CurrentGame
mkCurrentGame w s = CurrentGame w s New

data Intent =
    IntentSetup
  | IntentPlayGame !(Maybe LevelOutcome)
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
mkClients = Clients Map.empty (ShipId 0)

firstServerLevel :: LevelNumber
firstServerLevel = firstLevel

newServerState :: ServerLogs -> ColorScheme -> IO ServerState
newServerState logs colorScheme = do
  c <- mkCenterColor colorScheme
  let lvSpec = LevelSpec firstServerLevel CannotOvershoot
      params = initialParameters
  ServerState logs mkClients mkGameTiming lvSpec params
              (mkWorldCreation $ WorldSpec lvSpec Set.empty params)
              IntentSetup c False <$> newEmptyMVar

mkCenterColor :: ColorScheme -> IO (Color8 Foreground)
mkCenterColor (ColorScheme c) = return c
mkCenterColor UseServerStartTime = do
  t <- getCurrentSecond
  let !ref = rgb 3 2 0
      nColors = countHuesOfSameIntensity ref
      n = t `mod` nColors
  return $ rotateHue (fromIntegral n / fromIntegral nColors) ref
