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
      , ClientInfo(..)
      , Client, mkClient, unClient
      , takeClientId
      , mkClients
      , ServerLogs(..)
      , ClientHandlerIO
      , ServerOwnership(..)
      , DisconnectReason(..)
      , ServerState, unServerState, clientsMap
      , ServerImpl(..)
      , mapState
        -- * Client-side
        {- | Types used by a client to represent a server, and the connection to the server. -}
      , Server(..)
      , ServerName(..)
      , ServerPort(..)
      , ServerContent(..)
      , ConnectionStatus(..)
        -- * Events
      , ClientEvent(..)
      , ServerEvent(..)
      ) where

import           Imj.Prelude
import           Control.Monad.State.Strict(StateT, MonadState, runStateT, execStateT, modify', get, gets, state)
import qualified Data.Binary as Bin
import           Data.Int(Int64)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)
import           Data.String(IsString)
import           Data.Text.Lazy.Encoding as LazyT
import qualified Data.Text.Lazy as LazyT
import           Data.Text(unpack)
import           Network.WebSockets

import           Imj.Graphics.Color
import           Imj.Server.Internal.Types

--------------------------------------------------------------------------------
---------- Seen from a server's perspective ------------------------------------
--------------------------------------------------------------------------------

class (Show c) => ClientInfo c where
  clientLogColor :: c -> Maybe (Color8 Foreground)
  clientFriendlyName :: c -> Maybe Text

mkClients :: Clients c
mkClients = Clients Map.empty (ClientId 0)

takeClientId :: Clients c -> (Clients c, ClientId)
takeClientId (Clients c i) = (Clients c $ succ i, i)

type ClientHandlerIO s = StateT s (ReaderT ConstClient IO)

data ClientEvent e o =
    ClientAppEvt !e
  | Connect !o {-unpack sum-} !ServerOwnership
  deriving(Generic, Show)
instance (Binary e, Binary o) => Binary (ClientEvent e o)
instance (Binary e, Binary o) => WebSocketsData (ClientEvent e o) where
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
  | ConnectionAccepted {-# UNPACK #-} !ClientId
  | ConnectionRefused {-# UNPACK #-} !Text
  | Disconnected {-unpack sum-} !DisconnectReason
  | ServerError !String
  -- ^ A non-recoverable error occured in the server: before crashing, the server sends the error to its clients.
  deriving(Generic)
instance Show (ServerEventT s) => Show (ServerEvent s) where
  show _ = ""
instance Binary (ServerEventT e) => Binary (ServerEvent e)
instance Binary (ServerEventT s) => WebSocketsData (ServerEvent s) where
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

data Server cached = Server {
    serverType :: !ServerType
  , serverContent :: !(ServerContent cached)
}  deriving(Generic, Show)

data ServerContent cached = ServerContent {
    serverPort :: {-# UNPACK #-} !ServerPort
  , cachedContent :: !(Maybe cached)
    -- ^ To avoid querying the server when we know that the content didn't change.
}  deriving(Generic, Show)
