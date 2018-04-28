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
      , ConstClient(..)
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
      , ServerImpl(..)
      , DisconnectReason(..)
      ) where

import           Imj.Prelude
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.State.Strict(StateT, MonadState, runStateT, execStateT, modify', get, gets, state)
import           Data.Int(Int64)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)
import           Data.String(IsString)
import           Data.Text(pack,unpack)
import           Network.WebSockets

{-
type family ClientT a = r | r -> a
type family ServerEventT a = r | r -> a
type family ClientEventT a = r | r -> a
type family ConnectIdT a = r | r -> a
-}
class ServerImpl s where
  type ClientT s = (r :: *) | r -> s
  -- ^ Client definition.
  type ServerEventT s = (r :: *) | r -> s
  -- ^ Events sent by the server to the client(s).
  type ClientEventT s =  (r :: *)| r -> s
  -- ^ Events sent by clients to the server.
  type ConnectIdT s =  (r :: *) | r -> s
  -- ^ Some optional data that is used by the application to create the identity of the client,
  -- passed in 'Connect'.
  -- Typically, it can be a suggested player name.
  afterClientLeft :: (MonadIO m, MonadState (ServerState s) m)
                  => ServerEventT s -> ClientId -> DisconnectReason -> m ()


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

-- | A 'Server' handles one game only (for now).
data ServerState s = ServerState {
    serverLogs :: {-unpack sum-} !ServerLogs
  , getClients :: {-# UNPACK #-} !(Clients (ClientT s))
  , shouldTerminate :: {-unpack sum-} !Bool
  -- ^ Set on server shutdown
  , unServerState :: !s
} deriving(Generic)
instance (NFData s, NFData (ClientT s)) => NFData (ServerState s)
mapState :: (s -> s) -> ServerState s -> ServerState s
mapState f s = s { unServerState = f $ unServerState s }

{-# INLINE clientsMap #-}
clientsMap :: ServerState s -> Map ClientId (Client (ClientT s))
clientsMap = getClients' . getClients

data ServerLogs =
    NoLogs
  | ConsoleLogs
  deriving(Generic, Show)
instance NFData ServerLogs

data ServerType =
    Distant !ServerName
  | Local !ServerLogs
  deriving(Generic, Show)

newtype ServerName = ServerName String
  deriving (Show, IsString, Eq)

newtype ServerPort = ServerPort Int
  deriving (Generic, Show, Num, Integral, Real, Ord, Eq, Enum)

newtype ClientId = ClientId Int64
  deriving(Generic, Binary, Eq, Ord, Show, Enum, NFData)

data Clients c = Clients {
    getClients' :: !(Map ClientId (Client c))
    -- ^ Only connected clients are here: once a client is disconnected, it is removed from the Map.
  , getNextClientId :: !ClientId
    -- ^ The 'ClientId' that will be assigned to the next new client.
} deriving(Generic)
instance (NFData c) => NFData (Clients c)

-- | Immutable data associated to a client.
data ConstClient = ConstClient {
    connection :: {-# UNPACK #-} !Connection
  , clientId :: {-# UNPACK #-} !ClientId
}

data Client c = Client {
    getConnection :: {-# UNPACK #-} !Connection
  , getServerOwnership :: {-unpack sum-} !ServerOwnership
  , unClient :: !c
} deriving(Generic)
instance NFData c => NFData (Client c) where
  rnf (Client _ a b) = rnf a `seq` rnf b
instance Show c => Show (Client c) where
  show (Client _ a b) = show ("Client",a,b)
instance Functor Client where
  {-# INLINE fmap #-}
  fmap f c = c { unClient = f $ unClient c}

mkClient :: Connection -> ServerOwnership -> c -> Client c
mkClient a b c = Client a b c

data ServerOwnership =
    ClientOwnsServer
    -- ^ Implies that if the client is shutdown, the server is shutdown too.
  | ClientDoesntOwnServer
  deriving(Generic, Show, Eq)
instance Binary ServerOwnership
instance NFData ServerOwnership
