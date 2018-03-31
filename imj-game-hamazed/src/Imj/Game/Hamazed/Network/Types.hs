{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
This module exports types related to networking.

Game events are sent by the clients, proccessed by the server. For example, if two players
play the game:

@
  - Ax = acceleration of ship x
  - Lx = laser shot of ship x
  - .  = end of a game period

        >>> time >>>
 . . . A1 . . A1 A2 L2 L1 .
              ^^^^^ ^^^^^
              |     |
              |     laser shots can't be aggregated.
              |
              accelerations can be aggregated, their order within a period is unimportant.
@

The order in which L1 L2 are handled by the server is the order in which they are received.
This is /unfair/ because one player (due to network delays) could have rendered the
last period 100ms before the other, thus having a significant advantage over the other player.
We could be more fair by keeping track of the perceived time on the player side:

in 'ClientAction' we could store the difference between the system time of the action
and the system time at which the last motion update was presented to the player.

Hence, to know how to order close laser shots, if the ships are on the same row or column,
the server should wait a little (max. 50 ms?) to see if the other player makes a
perceptually earlier shot.
-}

module Imj.Game.Hamazed.Network.Types
      ( -- * ClientQueues
        ClientQueues(..)
      , addRequestAsync
      , removeRequestAsync
      , releaseRequestResources
      -- * Server
      , Server(..)
      , ServerType(..)
      , ServerContent(..)
      , ServerLogs(..)
      , ServerPort(..)
      , ServerName(..)
      , getServerNameAndPort
      -- * Player
      , SuggestedPlayerName(..)
      , PlayerName(..)
      , Player(..)
      , PlayerEssence(..)
      , mkPlayer
      , PlayerStatus(..) -- TODO should we merge with 'StateValue' ?
      , PlayerColors(..)
      , mkPlayerColors
      , getPlayerUIName'
      , getPlayerUIName''
      -- * Connection
      , ConnectionStatus(..)
      , NoConnectReason(..)
      , DisconnectReason(..)
      -- * Colors
      , ColorScheme(..)
      -- * Client
      , ServerOwnership(..)
      , ClientState(..)
      , StateNature(..)
      , StateValue(..)
      -- * Client / Server communication
      , EventsForClient(..)
      , ClientEvent(..)
      , ServerEvent(..)
      , WorldRequestArg(..)
      , ServerReport(..)
      , Command(..)
      , ClientCommand(..)
      , ServerCommand(..)
      , SharedValueKey(..)
      , SharedEnumerableValueKey(..)
      , SharedValue(..)
      , PlayerNotif(..)
      , GameNotif(..)
      , LeaveReason(..)
      , GameStep(..)
      , GameStatus(..)
      -- * Game
      , GameStateEssence(..)
      , ShotNumber(..)
      , Operation(..)
      , applyOperations
      -- * Utils
      , welcome
      ) where

import           Imj.Prelude

import           Control.Concurrent.Async (Async, cancel)
import qualified Control.Concurrent.MVar as Lazy(MVar, modifyMVar_) -- not using strict version, because Async misses NFData.
import           Control.Concurrent.STM(TQueue)
import           Control.DeepSeq(NFData)
import qualified Data.Map.Strict as Map(elems, alter, updateLookupWithKey)
import           Data.Map.Strict(Map)
import qualified Data.Binary as Bin(encode, decode)
import           Data.List(foldl')
import           Data.Set(Set)
import qualified Data.Set as Set(insert, delete, empty, null)
import           Data.String(IsString)
import           Data.Text(unpack)
import qualified Data.Text.Lazy as Lazy(unpack)
import           Data.Text.Lazy.Encoding as LazyE(decodeUtf8)
import           Network.WebSockets(WebSocketsData(..), DataMessage(..))

import           Imj.Game.Hamazed.Chat
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Font
import           Imj.Graphics.Text.ColorString(ColorString)
import qualified Imj.Graphics.Text.ColorString as ColorString(colored, intercalate)
import           Imj.Graphics.Text.ColoredGlyphList(ColoredGlyphList)
import qualified Imj.Graphics.Text.ColoredGlyphList as ColoredGlyphList(colored)
import           Imj.Timing

-- | a Server, seen from a Client's perspective
data Server = Server {
    serverType :: !ServerType
  , serverContent :: !ServerContent
}  deriving(Generic, Show)

data ServerType =
    Distant !ServerName
  | Local !ServerLogs !ColorScheme
  deriving(Generic, Show)

data ServerContent = ServerContent {
    serverPort :: {-# UNPACK #-} !ServerPort
  , serverWorldParameters :: !(Maybe WorldParameters)
}  deriving(Generic, Show)

data ServerLogs =
    NoLogs
  | ConsoleLogs
  deriving(Generic, Show)
instance NFData ServerLogs

data ColorScheme =
    UseServerStartTime
  | ColorScheme {-# UNPACK #-} !(Color8 Foreground)
  deriving(Generic, Show)
instance NFData ColorScheme


data ServerOwnership =
    ClientOwnsServer
    -- ^ Means if client is shutdown, server is shutdown too.
  | ClientDoesntOwnServer
  deriving(Generic, Show, Eq)
instance Binary ServerOwnership

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

-- | A client communicates with the server asynchronously, that-is, wrt the thread where
-- game state update and rendering occurs. Using 'TQueue' as a mean of communication
-- instead of 'MVar' has the benefit that in case of the connection being closed,
-- the main thread won't block.
data ClientQueues = ClientQueues { -- TODO Use -funbox-strict-fields to force deep evaluation of thunks when inserting in the queues
    inputQueue :: {-# UNPACK #-} !(TQueue EventsForClient)
  , outputQueue :: {-# UNPACK #-} !(TQueue ClientEvent)
  , requestsAsyncs :: !(Lazy.MVar RequestsAsyncs)
}

type RequestId = WorldId
type RequestsAsyncs = Map RequestId (Set (Async ()))

addRequestAsync :: Lazy.MVar RequestsAsyncs -> Async () -> RequestId -> IO ()
addRequestAsync r a wid =
  Lazy.modifyMVar_ r $ return . ($!) Map.alter alt wid
 where
  alt = Just . Set.insert a . fromMaybe Set.empty

removeRequestAsync :: Lazy.MVar RequestsAsyncs -> Async () -> RequestId -> IO ()
removeRequestAsync r a wid = Lazy.modifyMVar_ r $ return . ($!) Map.alter alt wid
 where
  alt = maybe
    Nothing
    (\set ->
      let s = Set.delete a set
      in bool (Just s) Nothing $ Set.null s)

releaseRequestResources :: Lazy.MVar RequestsAsyncs -> RequestId -> IO ()
releaseRequestResources r wid = Lazy.modifyMVar_ r $ \m -> do
  let (e, m') = Map.updateLookupWithKey (\_ _ -> Nothing) wid m
  maybe
    (return ())
    (mapM_ cancel)
    e
  return $! m'

data EventsForClient =
    FromClient !Event
  | FromServer !ServerEvent
  deriving(Generic, Show)

-- | An event generated by the client, sent to the server.
data ClientEvent =
    Connect !SuggestedPlayerName {-unpack sum-} !ServerOwnership
  | ExitedState {-unpack sum-} !StateValue
  | WorldProposal !WorldId !(MkSpaceResult WorldEssence) !(Maybe Statistics)
    -- ^ In response to 'WorldRequest' 'Build'
  | CurrentGameState {-# UNPACK #-} !WorldId !(Maybe GameStateEssence)
    -- ^ In response to 'WorldRequest' 'GetGameState'
  | IsReady {-# UNPACK #-} !WorldId
  -- ^ When the level's UI transition is finished.
  | Action {-unpack sum-} !ActionTarget {-unpack sum-} !Direction
   -- ^ A player action on an 'ActionTarget' in a 'Direction'.
  | LevelEnded {-unpack sum-} !LevelOutcome
  | CanContinue {-unpack sum-} !GameStatus
  -- NOTE the 3 constructors below could be factored as 'OnCommand' 'Command'
  | RequestApproval {-unpack sum-} !ClientCommand
  -- ^ A Client asks for authorization to run a 'ClientCommand'.
  -- In response the server either sends 'CommandError' to disallow command execution or 'RunCommand' to allow it.
  | Do {-unpack sum-} !ServerCommand
  -- ^ A Client asks the server to run a 'ServerCommand'.
  -- In response, the server runs the 'ServerCommand' then publishes a 'PlayerNotif' 'Done' 'ServerCommand'.
  | Report !ServerReport
  -- ^ A client want to know an information on the server state. The server will answer by
  -- sending a 'Report'.
  deriving(Generic, Show)
instance Binary ClientEvent
data ServerEvent =
    ConnectionAccepted {-# UNPACK #-} !ShipId
                                      !(Map ShipId PlayerEssence)
                       {-# UNPACK #-} !WorldParameters
  | ConnectionRefused {-# UNPACK #-} !NoConnectReason
  | Disconnected {-unpack sum-} !DisconnectReason
  | EnterState {-unpack sum-} !StateValue
  | ExitState {-unpack sum-} !StateValue
  | PlayerInfo {-unpack sum-} !PlayerNotif
               {-# UNPACK #-} !ShipId
  | GameInfo {-unpack sum-} !GameNotif
  | WorldRequest {-# UNPACK #-} !WorldId
                                !WorldRequestArg
  | ChangeLevel {-# UNPACK #-} !LevelEssence -- TODO merge with WorldRequest
                {-# UNPACK #-} !WorldEssence
                {-# UNPACK #-} !WorldId
  -- ^ Triggers a UI transition between the previous (if any) and the next level.
  | PutGameState {-# UNPACK #-} !GameStateEssence  -- TODO merge with WorldRequest
                 {-# UNPACK #-} !WorldId
  | OnWorldParameters {-# UNPACK #-} !WorldParameters
  -- ^ (reconnection scenario) Upon reception, the client should set its gamestate accordingly.
  | GameEvent {-unpack sum-} !GameStep
  | CommandError {-unpack sum-} !ClientCommand
                 {-# UNPACK #-} !Text
  -- ^ The command cannot be run, with a reason.
  | RunCommand {-# UNPACK #-} !ShipId
               {-unpack sum-} !ClientCommand
  -- ^ The server validated the use of the command, now it must be executed.
  | Reporting {-unpack sum-} !ServerCommand
  -- ^ Response to a 'Report'.
  | ServerError !String
  -- ^ A non-recoverable error occured in the server: before crashing, the server sends the error to its clients.
  deriving(Generic, Show)
instance Binary ServerEvent
instance WebSocketsData ClientEvent where
  fromDataMessage (Text t _) =
    error $ "Text was received for ClientEvent : " ++ Lazy.unpack (LazyE.decodeUtf8 t)
  fromDataMessage (Binary bytes) = Bin.decode bytes
  fromLazyByteString = Bin.decode
  toLazyByteString = Bin.encode
  {-# INLINABLE fromDataMessage #-}
  {-# INLINABLE fromLazyByteString #-}
  {-# INLINABLE toLazyByteString #-}
instance WebSocketsData ServerEvent where
  fromDataMessage (Text t _) =
    error $ "Text was received for ServerEvent : " ++ Lazy.unpack (LazyE.decodeUtf8 t)
  fromDataMessage (Binary bytes) = Bin.decode bytes
  fromLazyByteString = Bin.decode
  toLazyByteString = Bin.encode
  {-# INLINABLE fromDataMessage #-}
  {-# INLINABLE fromLazyByteString #-}
  {-# INLINABLE toLazyByteString #-}

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

data PlayerStatus = Present | Absent
  deriving(Generic, Show)
instance Binary PlayerStatus

data Player = Player {
    getPlayerName :: {-# UNPACK #-} !PlayerName
  , getPlayerStatus :: {-unpack sum-} !PlayerStatus
  , getPlayerColors :: {-# UNPACK #-} !PlayerColors
} deriving(Generic, Show)
instance Binary Player

data PlayerEssence = PlayerEssence {
    playerEssenceName :: {-# UNPACK #-} !PlayerName
  , playerEssenceStatus :: {-unpack sum-} !PlayerStatus
  , playerEssenceColor :: {-# UNPACK #-} !(Color8 Foreground)
} deriving(Generic, Show)
instance Binary PlayerEssence

mkPlayer :: PlayerEssence -> Player
mkPlayer (PlayerEssence a b color) =
  Player a b $ mkPlayerColors color

data PlayerColors = PlayerColors {
    getPlayerColor :: {-# UNPACK #-} !(Color8 Foreground)
    -- ^ color of player name and ship.
  , getColorCycles :: {-# UNPACK #-} !ColorCycles
    -- ^ colors for particle systems
} deriving(Generic, Show, Eq)
instance Binary PlayerColors

mkPlayerColors :: Color8 Foreground -> PlayerColors
mkPlayerColors c = PlayerColors c $ mkColorCycles c

getPlayerUIName' :: Maybe Player -> ColorString
getPlayerUIName' = getPlayerUIName ColorString.colored

getPlayerUIName'' :: Maybe Player -> ColoredGlyphList
getPlayerUIName'' = getPlayerUIName (ColoredGlyphList.colored . map textGlyph . unpack)

getPlayerUIName :: (IsString a, Monoid a)
                => (Text -> Color8 Foreground -> a)
                -> Maybe Player
                -> a
-- 'Nothing' happens when 2 players disconnect while playing: the first one to reconnect will not
-- know about the name of the other disconnected player, until the other player reconnects (TODO is it still the case?).
getPlayerUIName _ Nothing = "? (away)"
getPlayerUIName f (Just (Player (PlayerName name) status (PlayerColors c _))) =
  case status of
    Present -> n
    Absent  -> n <> f " (away)" chatMsgColor
 where
  n = f name c

data Command =
    ClientCmd !ClientCommand
  | ServerCmd !ServerCommand
  | ServerRep !ServerReport
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary Command

-- | Describes what the client wants to know about the server.
data ServerReport =
    Get !SharedValueKey
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary ServerReport

-- | Commands initiated by a client, executed by the server.
data ServerCommand =
    Put !SharedValue
  | Succ !SharedEnumerableValueKey
  | Pred !SharedEnumerableValueKey
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary ServerCommand

-- | Identifiers of values shared by all players.
data SharedEnumerableValueKey =
    BlockSize
  | WallProbability
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary SharedEnumerableValueKey

-- | Identifiers of values shared by all players.
data SharedValueKey =
    ColorSchemeCenterKey
  | WorldShapeKey
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary SharedValueKey

-- | Values shared by all players.
data SharedValue =
    ColorSchemeCenter {-# UNPACK #-} !(Color8 Foreground)
  | WorldShape {-unpack sum-} !WorldShape
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary SharedValue

-- | Commands initiated by /one/ client or the server, authorized (and in part executed) by the server,
--  then executed (for the final part) by /every/ client.
data ClientCommand =
    AssignName {-# UNPACK #-} !PlayerName
  | AssignColor {-# UNPACK #-} !(Color8 Foreground)
  | Says {-# UNPACK #-} !Text
  | Leaves {-unpack sum-} !LeaveReason
  -- ^ The client shuts down. Note that clients that are 'ClientOwnsServer',
  -- will also gracefully shutdown the server.
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary ClientCommand

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

applyOperations :: [ShotNumber] -> Int
applyOperations =
  foldl' (\v (ShotNumber n op) ->
            case op of
              Add -> v + n
              Substract -> v - n) 0
-- | 'PeriodicMotion' aggregates the accelerations of all ships during a game period.
data GameStep =
    PeriodicMotion {
    _shipsAccelerations :: !(Map ShipId (Coords Vel))
  , _shipsLostArmor :: !(Set ShipId)
}
  | LaserShot {-unpack sum-} !Direction {-# UNPACK #-} !ShipId
  deriving(Generic, Show)
instance Binary GameStep

data ConnectionStatus =
    NotConnected
  | Connected {-# UNPACK #-} !ShipId
  | ConnectionFailed {-# UNPACK #-} !NoConnectReason

data NoConnectReason =
    InvalidName !SuggestedPlayerName {-# UNPACK #-} !Text
  deriving(Generic, Show)
instance Binary NoConnectReason

data DisconnectReason =
    BrokenClient {-# UNPACK #-} !Text
    -- ^ One client is disconnected because its connection is unusable.
  | ClientShutdown
    -- ^ One client is disconnected because it decided so.
  | ServerShutdown {-# UNPACK #-} !Text
  -- ^ All clients are disconnected.
  deriving(Generic)
instance Binary DisconnectReason
instance Show DisconnectReason where
  show (ServerShutdown t) = unpack $ "Server shutdown < " <> t
  show ClientShutdown   = "Client shutdown"
  show (BrokenClient t) = unpack $ "Broken client < " <> t

data PlayerNotif =
    Joins
  | WaitsToJoin
  | StartsGame
  | Done {-unpack sum-} !ServerCommand
    -- ^ The server notifies whenever a 'Do' task is finished.
  deriving(Generic, Show)
instance Binary PlayerNotif

data LeaveReason =
    ConnectionError !Text
  | Intentional
  deriving(Generic, Show, Eq)
instance Binary LeaveReason

data GameNotif =
    LevelResult {-# UNPACK #-} !LevelNumber {-unpack sum-} !LevelOutcome
  | GameWon
  | CannotCreateLevel ![Text] {-# UNPACK #-} !LevelNumber
  deriving(Generic, Show)
instance Binary GameNotif

welcome :: Map ShipId Player -> ColorString
welcome l =
  text "Welcome! Players are: "
  <> ColorString.intercalate
      (text ", ")
      (map (getPlayerUIName' . Just) $ Map.elems l)
 where
  text x = ColorString.colored x chatMsgColor

newtype SuggestedPlayerName = SuggestedPlayerName String
  deriving(Generic, Eq, Show, Binary, IsString)


getServerNameAndPort :: Server -> (ServerName, ServerPort)
getServerNameAndPort (Server (Local _ _) (ServerContent p _)) = (ServerName "localhost", p)
getServerNameAndPort (Server (Distant name) (ServerContent p _)) = (name, p)

newtype ServerName = ServerName String
  deriving (Show, IsString, Eq)

newtype ServerPort = ServerPort Int
  deriving (Generic, Show, Num, Integral, Real, Ord, Eq, Enum)
