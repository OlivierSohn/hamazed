{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Server.Types
      ( Server(..)
      , ServerState, unServerState, clientsMap
      , mkServerState
      , ServerEvent(..)
      , ClientEvent(..)
      , Command(..)
      , ClientCommand(..)
      , ClientName(..), unClientName
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      , PlayerNotif(..)
      ) where

import           Imj.Prelude
import qualified Data.Binary as Bin
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)
import           Data.Text.Lazy.Encoding as LazyT
import qualified Data.Text.Lazy as LazyT
import           Network.WebSockets

import           Imj.Categorized
import           Imj.Server.Internal.Types
import           Imj.Server.Class
import           Imj.Graphics.Color

data Command s =
    RequestApproval !ClientCommand
  -- ^ A Client asks for authorization to run a 'ClientCommand'.
  -- In response the server either sends 'CommandError' to disallow command execution or 'RunCommand' to allow it.
  | Do !(ServerCommand s)
  -- ^ A Client asks the server to run a 'ServerCommand'.
  -- In response, the server runs the 'ServerCommand' then publishes a 'PlayerNotif' 'Done' 'ServerCommand'.
  | Report !(ServerReport s)
  -- ^ A client want to know an information on the server state. The server will answer by
  -- sending a 'Report'.
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Server s => Binary (Command s)

data ClientEvent s =
    ClientAppEvt !(ClientEventT s)
  | Connect !(ConnectIdT s) {-unpack sum-} !ServerOwnership
  | OnCommand !(Command s)
  deriving(Generic)
instance Server s => Binary (ClientEvent s)
instance Server s => WebSocketsData (ClientEvent s) where
  fromDataMessage (Text t _) =
    error $ "Text was received for ClientEvent : " ++ LazyT.unpack (LazyT.decodeUtf8 t)
  fromDataMessage (Binary bytes) = Bin.decode bytes
  fromLazyByteString = Bin.decode
  toLazyByteString = Bin.encode
  {-# INLINABLE fromDataMessage #-}
  {-# INLINABLE fromLazyByteString #-}
  {-# INLINABLE toLazyByteString #-}
instance Server s => Show (ClientEvent s) where
  show (ClientAppEvt e) = show ("ClientAppEvt" ,e)
  show (Connect s e) = show ("Connect" ,s,e)
  show (OnCommand x) = show ("OnCommand" ,x)

data ServerEvent s =
    ServerAppEvt !(ServerEventT s)
  | CommandError {-unpack sum-} !ClientCommand
                 {-# UNPACK #-} !Text
  -- ^ The command cannot be run, with a reason.
  | RunCommand {-# UNPACK #-} !ClientId
               {-unpack sum-} !ClientCommand
  -- ^ The server validated the use of the command, now it must be executed.
  | Reporting {-unpack sum-} !(ServerCommand s)
  -- ^ Response to a 'Report'.
  | PlayerInfo !(PlayerNotif s)
               {-# UNPACK #-} !ClientId
  | ConnectionAccepted {-# UNPACK #-} !ClientId
  | ConnectionRefused !(ConnectIdT s) {-# UNPACK #-} !Text
  | Disconnected {-unpack sum-} !DisconnectReason
  | OnContent !(ServerViewContentT s)
  -- ^ Sent to every newly connected client, and to all clients whenever the content changes.
  | ServerError !String
  -- ^ A non-recoverable error occured in the server: before crashing, the server sends the error to its clients.
  deriving(Generic, Show)
instance Server s => Binary (ServerEvent s)
instance Server s => WebSocketsData (ServerEvent s) where
  fromDataMessage (Text t _) =
    error $ "Text was received for ServerEvent : " ++ LazyT.unpack (LazyT.decodeUtf8 t)
  fromDataMessage (Binary bytes) = Bin.decode bytes
  fromLazyByteString = Bin.decode
  toLazyByteString = Bin.encode
  {-# INLINABLE fromDataMessage #-}
  {-# INLINABLE fromLazyByteString #-}
  {-# INLINABLE toLazyByteString #-}
instance Server s => Categorized (ServerEvent s) where
  evtCategory = \case
    Reporting _ -> Chat'
    PlayerInfo _ _ -> Chat'
    ServerError _ -> Error'
    Disconnected _ -> Disconnected'
    ConnectionAccepted {} -> ConnectionAccepted'
    ConnectionRefused {} -> ConnectionRefused'
    CommandError _ _ -> Error'
    RunCommand _ _ -> WorldRequest'
    OnContent _ -> Chat'
    ServerAppEvt e -> evtCategory e

-- | Commands initiated by /one/ client or the server, authorized (and in part executed) by the server,
--  then executed (for the final part) by /every/ client.
data ClientCommand =
    AssignName {-# UNPACK #-} !ClientName
  | AssignColor {-# UNPACK #-} !(Color8 Foreground)
  | Says {-# UNPACK #-} !Text
  | Leaves {-unpack sum-} !(Either Text ())
  -- ^ The client shuts down. Note that clients that are 'ClientOwnsServer',
  -- will also gracefully shutdown the server.
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary ClientCommand

data PlayerNotif s =
    Joins
  | WaitsToJoin
  | StartsGame
  | Done {-unpack sum-} !(ServerCommand s)
    -- ^ The server notifies whenever a 'Do' task is finished.
  deriving(Generic, Show)
instance Server s => Binary (PlayerNotif s)

mkServerState :: ServerLogs -> ServerViewContentT s -> s -> ServerState s
mkServerState logs c s =
  ServerState logs (ClientViews Map.empty (ClientId 0)) False c s

{-# INLINE clientsMap #-}
clientsMap :: ServerState s -> Map ClientId (ClientView (ClientViewT s))
clientsMap = views . clientsViews
