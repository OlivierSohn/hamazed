{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.ClientServer.Types
      ( ClientServer(..)
      , ServerState(..)
      , ServerEvent(..)
      , Clients(..)
      , Client(..)
      , ClientInfo(..)
      , ClientId(..)
      , ClientEvent(..)
      , ConstClient(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      ) where

import           Imj.Prelude
import qualified Data.Binary as Bin
import           Data.Text.Lazy.Encoding as LazyT
import qualified Data.Text.Lazy as LazyT
import           Network.WebSockets

import           Imj.ClientServer.Internal.Types
import           Imj.ClientServer.Class

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
  | ConnectionRefused !(ConnectIdT s) {-# UNPACK #-} !Text
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
