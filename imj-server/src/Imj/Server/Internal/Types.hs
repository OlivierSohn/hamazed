{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Imj.Server.Internal.Types
      ( ClientId(..)
      , ServerState(..)
      , mapState
      , clientsMap
      , Clients(..)
      , Client(..)
      , mkClient
      , ServerOwnership(..)
      , ServerLogs(..)
      , ServerType(..)
      , ServerName(..)
      , ServerPort(..)
      , DisconnectReason(..)
      ) where

import           Imj.Prelude
import           Data.Map.Strict(Map)
import           Data.String(IsString)
import           Network.WebSockets(Connection)

import           Imj.ClientServer.Class

{-# INLINE clientsMap #-}
clientsMap :: ServerState s -> Map ClientId (Client (ClientT s))
clientsMap = getClients' . getClients

{-# INLINE mapState #-}
mapState :: (s -> s) -> ServerState s -> ServerState s
mapState f s = s { unServerState = f $ unServerState s }

data ServerType p =
    Distant !ServerName
  | Local !ServerLogs !p
  deriving(Generic, Show)

newtype ServerName = ServerName String
  deriving (Show, IsString, Eq)

newtype ServerPort = ServerPort Int
  deriving (Generic, Show, Num, Integral, Real, Ord, Eq, Enum)


mkClient :: Connection -> ServerOwnership -> c -> Client c
mkClient a b c = Client a b c
