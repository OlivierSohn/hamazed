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

module Imj.ClientServer.Class
      ( ClientServer(..)
      , ServerState(..)
      , Clients(..)
      , Client(..)
      , ClientInfo(..)
      , ClientId(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      , ClientHandlerIO
      ) where

import           Imj.Prelude
import           Control.Concurrent.MVar.Strict (MVar)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.State.Strict(MonadState, StateT)

import           Imj.ClientServer.Internal.Types

import           Imj.Graphics.Color

-- | Server-side info concerning a client.
class (Show c) => ClientInfo c where
  clientLogColor :: c -> Maybe (Color8 Foreground)
  clientFriendlyName :: c -> Maybe Text

type ClientHandlerIO s = StateT (ServerState s) (ReaderT ConstClient IO)

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
  ClientServer s -- TODO rename to Server? only the server needs an instance of this, eventhough
  -- some of the types concern both the client and the server.
 where

  -------------- [Server <--> Client] Messages ---------------------------------
  type ServerEventT s = (r :: *) | r -> s
  -- ^ [Server --> Client] events
  type ClientEventT s = (r :: *)| r -> s
  -- ^ [Client --> Server] events
  type ConnectIdT s = (r :: *) | r -> s
  -- ^ Passed in 'ClientEvent' 'Connect'.

  -------------- Server-side ---------------------------------------------------
  -- | "Server-side" client definition.
  type ClientT s = (r :: *) | r -> s
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

  createClient :: (MonadIO m, MonadState (ServerState s) m)
               => ClientId
               -> ConnectIdT s
               -> m (ClientT s)

  -- | These events are sent to connected clients when a new client is about
  -- to be added.
  eventsOnWillAddClient :: ClientId -> ClientT s -> [ServerEventT s]

  -- | These events are sent to the newly added client.
  greetings :: (MonadIO m, MonadState (ServerState s) m)
            => m [ServerEventT s]

  -- | Called once, when 'tryReconnect' returned a 'Just', and before any call to
  -- 'handleClientEvent'
  onReconnection :: ReconnectionContext s -> (ClientHandlerIO s) ()

  handleClientEvent :: ClientEventT s -> (ClientHandlerIO s) ()

  -- | Called after a client was disconnected.
  afterClientLeft :: (MonadIO m, MonadState (ServerState s) m)
                  => ClientId
                  -> DisconnectReason -> m ()

data ServerState s = ServerState {
    serverLogs :: {-unpack sum-} !ServerLogs
  , getClients :: {-# UNPACK #-} !(Clients (ClientT s))
  , shouldTerminate :: {-unpack sum-} !Bool
  -- ^ Set on server shutdown
  , unServerState :: !s
} deriving(Generic)
instance (ClientServer s) => NFData (ServerState s)
