{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.ClientServer.Internal.Types
      ( Clients(..)
      , Client(..)
      , ConstClient(..)
      , ClientId(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      , ClientLifecycle(..)
      ) where

import           Imj.Prelude
import           Data.Int(Int64)
import           Data.Map.Strict(Map)
import           Data.Text(unpack)
import           Network.WebSockets(Connection)


data ClientLifecycle c =
    NewClient
  | ReconnectingClient !c
  deriving(Show)

-- | Immutable data associated to a client.
data ConstClient = ConstClient {
    connection :: {-# UNPACK #-} !Connection
  , clientId :: {-# UNPACK #-} !ClientId
}

data Clients c = Clients {
    getClients' :: !(Map ClientId (Client c))
    -- ^ Only connected clients are here: once a client is disconnected, it is removed from the Map.
  , getNextClientId :: !ClientId
    -- ^ The 'ClientId' that will be assigned to the next new client.
} deriving(Generic)
instance (NFData c) => NFData (Clients c)

data ServerLogs =
    NoLogs
  | ConsoleLogs
  deriving(Generic, Show)
instance NFData ServerLogs

newtype ClientId = ClientId Int64
  deriving(Generic, Binary, Eq, Ord, Show, Enum, NFData, Integral, Real, Num)

data Client c = Client {
    getConnection :: {-# UNPACK #-} !Connection
  , getServerOwnership :: {-unpack sum-} !ServerOwnership
  , unClient :: !c
} deriving(Generic)
instance NFData c => NFData (Client c) where
  rnf (Client _ a b) = rnf a `seq` rnf b
instance Show c => Show (Client c) where
  show (Client _ a b) = show ("Client" :: String,a,b)
instance Functor Client where
  {-# INLINE fmap #-}
  fmap f c = c { unClient = f $ unClient c}

data ServerOwnership =
    ClientOwnsServer
    -- ^ Implies that if the client is shutdown, the server is shutdown too.
  | ClientDoesntOwnServer
  deriving(Generic, Show, Eq)
instance Binary ServerOwnership
instance NFData ServerOwnership

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
