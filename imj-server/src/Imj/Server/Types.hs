{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Imj.Server.Types
      (
        -- * Server-side
        {- | Types used by a server to represent itself. -}
        ClientId
      , ConstClient
      , Clients
      , Client, mkClient, unClient
      , takeClientId
      , ServerLogs(..)
      , ClientHandlerIO
      , ServerOwnership(..)
      , DisconnectReason(..)
      , ServerState, unServerState, clientsMap
      , mkServerState
      , ClientServer(..)
      , mapState
        -- * Client-side
        {- | Types used by a client to represent a server, and the connection to the server. -}
      , Server(..)
      , ServerType(..)
      , ServerName(..)
      , ServerPort(..)
      , ServerContent(..)
      , ConnectionStatus(..)
        -- * Events
      , ClientEvent(..)
      , ServerEvent(..)
      ) where

import           Imj.Prelude
import           Control.Monad.State.Strict(StateT)
import qualified Data.Binary as Bin
import qualified Data.Map.Strict as Map
import           Data.Text.Lazy.Encoding as LazyT
import qualified Data.Text.Lazy as LazyT
import           Network.WebSockets

import           Imj.ClientServer.Class
import           Imj.Server.Internal.Types

--------------------------------------------------------------------------------
---------- Seen from a server's perspective ------------------------------------
--------------------------------------------------------------------------------

mkEmptyClients :: Clients c
mkEmptyClients = Clients Map.empty (ClientId 0)

mkServerState :: ServerLogs -> s -> ServerState s
mkServerState logs s =
  ServerState logs mkEmptyClients False s

takeClientId :: Clients c -> (Clients c, ClientId)
takeClientId (Clients c i) = (Clients c $ succ i, i)

type ClientHandlerIO s = StateT (ServerState s) (ReaderT ConstClient IO)

data ClientEvent s =
    ClientAppEvt !(ClientEventT s)
  | Connect !(ConnectIdT s) {-unpack sum-} !ServerOwnership
  deriving(Generic)
instance ClientServer s => Binary (ClientEvent s)
instance ClientServer s => WebSocketsData (ClientEvent s) where
  fromDataMessage (Text t _) =
    error $ "Text was received for ClientEvent : " ++ LazyT.unpack (LazyT.decodeUtf8 t)
  fromDataMessage (Binary bytes) = Bin.decode bytes
  fromLazyByteString = Bin.decode
  toLazyByteString = Bin.encode
  {-# INLINABLE fromDataMessage #-}
  {-# INLINABLE fromLazyByteString #-}
  {-# INLINABLE toLazyByteString #-}
instance ClientServer s => Show (ClientEvent s) where
  show (ClientAppEvt e) = show ("ClientAppEvt" :: String,e)
  show (Connect s e) = show ("Connect" :: String,s,e)

data ServerEvent s =
    ServerAppEvt !(ServerEventT s)
  | ConnectionAccepted {-# UNPACK #-} !ClientId
  | ConnectionRefused {-# UNPACK #-} !Text
  | Disconnected {-unpack sum-} !DisconnectReason
  | ServerError !String
  -- ^ A non-recoverable error occured in the server: before crashing, the server sends the error to its clients.
  deriving(Generic)
instance ClientServer s => Show (ServerEvent s) where
  show _ = ""
instance ClientServer s => Binary (ServerEvent s)
instance ClientServer s => WebSocketsData (ServerEvent s) where
  fromDataMessage (Text t _) =
    error $ "Text was received for ServerEvent : " ++ LazyT.unpack (LazyT.decodeUtf8 t)
  fromDataMessage (Binary bytes) = Bin.decode bytes
  fromLazyByteString = Bin.decode
  toLazyByteString = Bin.encode
  {-# INLINABLE fromDataMessage #-}
  {-# INLINABLE fromLazyByteString #-}
  {-# INLINABLE toLazyByteString #-}


--------------------------------------------------------------------------------
---------- Seen from a client's perspective ------------------------------------
--------------------------------------------------------------------------------

data ConnectionStatus =
    NotConnected
  | Connected {-# UNPACK #-} !ClientId
  | ConnectionFailed {-# UNPACK #-} !Text

data Server param cached = Server {
    serverType :: !(ServerType param)
  , serverContent :: !(ServerContent cached)
}  deriving(Generic, Show)

data ServerContent cached = ServerContent {
    serverPort :: {-# UNPACK #-} !ServerPort
  , cachedContent :: !(Maybe cached)
    -- ^ To avoid querying the server when we know that the content didn't change.
}  deriving(Generic, Show)
