{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Server.Class
      ( Server(..)
      , ServerState(..)
      , ClientViews(..)
      , ClientView(..)
      , ClientInfo(..)
      , ClientId(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      -- * reexports
      , MonadReader
      , MonadState
      ) where

import           Imj.Prelude
import           Control.Concurrent.MVar.Strict (MVar)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad.State.Strict(MonadState)

import           Imj.Server.Internal.Types

import           Imj.Graphics.Color

-- | Info concerning a client.
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
     , NFData (ClientViewT s)
     , ClientInfo (ClientViewT s)
     )
 =>
  Server s
 where

  -------------- [Server <--> Client] Messages ---------------------------------
  type ServerEventT s = (r :: *) | r -> s
  -- ^ [Server --> Client] events
  type ClientEventT s = (r :: *)| r -> s
  -- ^ [Client --> Server] events
  type ConnectIdT s = (r :: *) | r -> s
  -- ^ Passed in 'ClientEvent' 'Connect'.

  -- | "Server-side" client definition.
  type ClientViewT s = (r :: *) | r -> s
  -- | Some data used when a client is reconnecting.
  type ReconnectionContext s

  -- | Returns actions that are not associated to a particular client, and that
  -- need to be run as long as the server is running. For a game server,
  -- the list will typically contain a game scheduling action.
  inParallel :: [MVar (ServerState s) -> IO ()]

  -- | When returning Left, the corresponding client connection is rejected.
  acceptConnection :: ConnectIdT s -> Either Text ()

  -- | Return 'Just' if the client identified by its 'ConnectIdT' should be considered reconnecting.
  tryReconnect :: (MonadIO m, MonadState (ServerState s) m)
               => ConnectIdT s
               -> m (Maybe (ClientId, ReconnectionContext s))

  -- | Creates the client view, given the 'ClientId' and 'ConnectIdT'.
  createClientView :: (MonadIO m, MonadState (ServerState s) m)
                   => ClientId
                   -> ConnectIdT s
                   -> m (ClientViewT s)

  -- | These events are sent to every connected clients when a new client is about
  -- to be added.
  announceNewcomer :: ClientId -> ClientViewT s -> [ServerEventT s]

  -- | These events are sent to the newly added client.
  greetNewcomer :: (MonadIO m, MonadState (ServerState s) m)
                => m [ServerEventT s]

  -- | For reconnection scenario : called once, only if 'tryReconnect' returned a 'Just'.
  onReconnection :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClient m)
                 => ReconnectionContext s -> m ()

  -- | Handle an incoming client event.
  handleClientEvent :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClient m)
                    => ClientEventT s -> m ()

  -- | Called after a client has been disconnected (either intentionally or on connection error).
  afterClientLeft :: (MonadIO m, MonadState (ServerState s) m)
                  => ClientId
                  -> DisconnectReason -> m ()

data ServerState s = ServerState {
    serverLogs :: {-unpack sum-} !ServerLogs
  , clientsViews :: {-# UNPACK #-} !(ClientViews (ClientViewT s))
  , shouldTerminate :: {-unpack sum-} !Bool
  -- ^ Set on server shutdown
  , unServerState :: !s
} deriving(Generic)
instance (Server s) => NFData (ServerState s)
