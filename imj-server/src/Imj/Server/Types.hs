{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Server.Types
      ( Server(..)
      , ServerState, unServerState, clientsMap
      , mkServerState
      , ServerEvent(..)
      , ClientEvent(..)
      , ClientLifecycle(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      , PlayerNotif(..)
      , StateValue(..)
      ) where

import           Imj.Prelude
import qualified Data.Binary as Bin
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)
import           Data.Set(Set)
import           Data.Text.Lazy.Encoding as LazyT
import qualified Data.Text.Lazy as LazyT
import           Network.WebSockets

import           Imj.Categorized
import           Imj.ClientView.Types
import           Imj.Graphics.Color
import           Imj.Music
import           Imj.Network
import           Imj.Server.Internal.Types
import           Imj.Server.Class

data ClientEvent s =
    ClientAppEvt !(ClientEventT s)
  | Connect !(Set MAC) !(Maybe (ConnectIdT s)) {-unpack sum-} !ServerOwnership
  | ExitedState {-unpack sum-} !(StateValue (StateValueT s))
  | OnCommand !(Command s)
  deriving(Generic)
instance (Server s, ServerClientHandler s) => Show (ClientEvent s) where
  show = \case
    ClientAppEvt x -> show ("ClientAppEvt",x)
    Connect x y z -> show ("Connect",x,y,z)
    ExitedState x -> show ("ExitedState",x)
    OnCommand x -> show ("OnCommand",x)
instance (Server s, ServerClientHandler s) => Binary (ClientEvent s)
instance (Server s, ServerClientHandler s) => WebSocketsData (ClientEvent s) where
  fromDataMessage (Text t _) =
    error $ "Text was received for ClientEvent : " ++ LazyT.unpack (LazyT.decodeUtf8 t)
  fromDataMessage (Binary bytes) = Bin.decode bytes
  fromLazyByteString = Bin.decode
  toLazyByteString = Bin.encode
  {-# INLINABLE fromDataMessage #-}
  {-# INLINABLE fromLazyByteString #-}
  {-# INLINABLE toLazyByteString #-}

data ServerEvent s =
    ServerAppEvt !(ServerEventT s)
  | PlayMusic !Music !Instrument
  | CommandError {-unpack sum-} !(ClientCommand (CustomCmdT s) Proposed)
                 {-# UNPACK #-} !Text
  -- ^ The command cannot be run, with a reason.
  | RunCommand {-# UNPACK #-} !ClientId
               {-unpack sum-} !(ClientCommand (CustomCmdT s) Approved)
  -- ^ The server validated the use of the command, now it must be executed.
  | Reporting {-unpack sum-} !(ServerCommand (ValueT s) (EnumValueKeyT s))
  -- ^ Response to a 'Report'.
  | PlayerInfo !(PlayerNotif (ValueT s) (EnumValueKeyT s))
               {-# UNPACK #-} !ClientId
  | ConnectionAccepted {-# UNPACK #-} !ClientId
  | ConnectionRefused !(Maybe (ConnectIdT s)) {-# UNPACK #-} !Text
  | Disconnected {-unpack sum-} !DisconnectReason
  | OnContent !(ValuesT s)
  -- ^ Sent to every newly connected client, and to all clients whenever the content changes.
  | AllClients !(Map ClientId ClientEssence)
  | EnterState {-unpack sum-} !(StateValue (StateValueT s))
  | ExitState {-unpack sum-} !(StateValue (StateValueT s))
  | ServerError !String
  -- ^ A non-recoverable error occured in the server: before crashing, the server sends the error to its clients.
  deriving(Generic)
instance (Server s, ServerClientHandler s) => Show (ServerEvent s) where
  show = \case
    ServerAppEvt x -> show ("ServerAppEvt",x)
    PlayMusic x y -> show ("PlayMusic",x,y)
    CommandError x y -> show ("CommandError",x, y)
    RunCommand x y -> show ("RunCommand",x, y)
    Reporting x -> show ("Reporting",x)
    PlayerInfo x y -> show ("PlayerInfo",x, y)
    ConnectionAccepted x -> show ("ConnectionAccepted",x)
    ConnectionRefused x y -> show ("ConnectionRefused",x,y)
    Disconnected x -> show ("Disconnected",x)
    OnContent x -> show ("OnContent",x)
    AllClients x -> show ("AllClients",x)
    EnterState x -> show ("EnterState",x)
    ExitState x -> show ("ExitState",x)
    ServerError x -> show ("ServerError",x)
instance (Server s, ServerClientHandler s) => Binary (ServerEvent s)
instance (Server s, ServerClientHandler s) => WebSocketsData (ServerEvent s) where
  fromDataMessage (Text t _) =
    error $ "Text was received for ServerEvent : " ++ LazyT.unpack (LazyT.decodeUtf8 t)
  fromDataMessage (Binary bytes) = Bin.decode bytes
  fromLazyByteString = Bin.decode
  toLazyByteString = Bin.encode
  {-# INLINABLE fromDataMessage #-}
  {-# INLINABLE fromLazyByteString #-}
  {-# INLINABLE toLazyByteString #-}
instance (Server s, ServerClientHandler s) => Categorized (ServerEvent s) where
  evtCategory = \case
    PlayMusic{} -> Command'
    Reporting _ -> Chat'
    PlayerInfo _ _ -> Chat'
    ServerError _ -> Error'
    Disconnected _ -> Disconnected'
    ConnectionAccepted {} -> ConnectionAccepted'
    ConnectionRefused {} -> ConnectionRefused'
    CommandError _ _ -> Error'
    RunCommand _ _ -> WorldRequest'
    OnContent _ -> Chat'
    AllClients{} -> Chat'
    EnterState _ -> EnterState'
    ExitState _ -> ExitState'
    ServerAppEvt e -> evtCategory e

data StateValue s =
    Excluded
    -- ^ The client is not part of the game
  | Included !s
  -- ^ The client is part of the game
  deriving(Generic, Show, Eq)
instance Binary s => Binary (StateValue s)
instance NFData s => NFData (StateValue s)

data PlayerNotif v e =
    Joins
  | WaitsToJoin
  | StartsGame
  | Done {-unpack sum-} !(ServerCommand v e)
    -- ^ The server notifies whenever a 'Do' task is finished.
  deriving(Generic, Show)
instance (Binary v, Binary e) => Binary (PlayerNotif v e)

mkServerState :: ServerLogs -> Color8 Foreground -> ValuesT s -> s -> ServerState s
mkServerState logs color c s =
  ServerState logs (ClientViews Map.empty Map.empty (ClientId 0)) False c color s

{-# INLINE clientsMap #-}
clientsMap :: ServerState s -> Map ClientId (ClientView (ClientViewT s))
clientsMap = views . clientsViews
