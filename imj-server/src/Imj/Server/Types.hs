{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Server.Types
      ( Server(..)
      , ServerState, unServerState, clientsMap
      , mkServerState
      , ServerEvent(..)
      , ClientInfo(..)
      , ClientEvent(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
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

data ClientEvent s =
    ClientAppEvt !(ClientEventT s)
  | Connect !(ConnectIdT s) {-unpack sum-} !ServerOwnership
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
instance Server s => Show (ServerEvent s) where
  show _ = ""
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
    ServerError _ -> Error'
    Disconnected _ -> Disconnected'
    ConnectionAccepted {} -> ConnectionAccepted'
    ConnectionRefused {} -> ConnectionRefused'
    ServerAppEvt e -> evtCategory e

mkServerState :: ServerLogs -> s -> ServerState s
mkServerState logs s =
  ServerState logs (ClientViews Map.empty (ClientId 0)) False s

{-# INLINE clientsMap #-}
clientsMap :: ServerState s -> Map ClientId (ClientView (ClientViewT s))
clientsMap = views . clientsViews
