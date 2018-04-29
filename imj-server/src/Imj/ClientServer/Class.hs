{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Imj.ClientServer.Class
      ( ClientServer(..)
      -- * Server-side types
      , ServerState(..)
      , Clients(..)
      , Client(..)
      , ClientInfo(..)
      , ClientId(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      -- * Client-side types
      ) where

import           Imj.Prelude
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.State.Strict(MonadState)
import           Data.Int(Int64)
import           Data.Map.Strict(Map)
import           Data.Text(unpack)
import           Network.WebSockets

import           Imj.Graphics.Color

-- | Server-side info concerning a client.
class (Show c) => ClientInfo c where
  clientLogColor :: c -> Maybe (Color8 Foreground)
  clientFriendlyName :: c -> Maybe Text

class (Show (ClientEventT s)
     , Show (ServerEventT s)
     , Show (ConnectIdT s)
     , Binary (ClientEventT s)
     , Binary (ServerEventT s)
     , Binary (ConnectIdT s)
     , NFData s
     , NFData (ClientT s)
     , ClientInfo (ClientT s)
     )
 =>
  ClientServer s
 where

  -------------- [Server <--> Client] Messages ---------------------------------
  type ServerEventT s = (r :: *) | r -> s
  -- ^ [Server --> Client] events
  type ClientEventT s = (r :: *)| r -> s
  -- ^ [Client --> Server] events
  type ConnectIdT s = (r :: *) | r -> s -- TODO this should be part of 'ClientEventT'
  -- ^ Some optional data that is used by the application to create the identity of the client,
  -- passed in 'ClientEventT' 'Connect'.

  -------------- Server-side ---------------------------------------------------
  -- | "Server-side" client definition.
  type ClientT s = (r :: *) | r -> s

  -- TODO use this:
{-
  connectIdIsValid :: ConnectIdT s -> Either Text ()

  makeClientId :: (MonadIO m, MonadState (ServerState s) m) => m (ClientId,a)

  handleClient :: ConnectIdT s -> ServerOwnership -> a ->



  onConnectionEvent :: (MonadIO m, MonadState (ServerState s) m)
                    => ServerOwnerShip
                    -> ConnectIdT s
                    -> m ()
  onApplicationEvent :: (MonadIO m, MonadState (ServerState s) m)
                     => ClientEventT s
                     -> m ()
-}
  -- | Called server-side, after a client was disconnected.
  afterClientLeft :: (MonadIO m, MonadState (ServerState s) m)
                  => ClientId -> DisconnectReason -> m ()

-- | A 'Server' handles one game only (for now).
data ServerState s = ServerState {
    serverLogs :: {-unpack sum-} !ServerLogs
  , getClients :: {-# UNPACK #-} !(Clients (ClientT s))
  , shouldTerminate :: {-unpack sum-} !Bool
  -- ^ Set on server shutdown
  , unServerState :: !s
} deriving(Generic)
instance (ClientServer s) => NFData (ServerState s)

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
  deriving(Generic, Binary, Eq, Ord, Show, Enum, NFData)

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
