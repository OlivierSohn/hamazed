{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Server.Class
      ( Server(..)
      , ServerReport(..)
      , ServerCommand(..)
      , ServerState(..)
      , ClientViews(..)
      , ClientView(..)
      , ClientId(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      , ClientName(..)
      , unClientName
      , ChatShow(..)
      -- * reexports
      , MonadReader
      , MonadState
      ) where

import           Imj.Prelude
import           Data.List(unwords)
import           Control.Concurrent.MVar.Strict (MVar)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad.State.Strict(MonadState)

import           Imj.Categorized
import           Imj.ClientView.Internal.Types
import           Imj.Graphics.Class.UIInstructions
import           Imj.Server.Internal.Types

import           Imj.Graphics.Color

class (Show (ClientEventT s)
     , Show (ConnectIdT s)
     , Show (ServerViewParamT s)
     , Show (ServerViewContentT s)
     , Show (ClientViewT s)
     , Show (SharedValueKeyT s)
     , Show (SharedValueT s)
     , Show (SharedEnumerableValueKeyT s)
     , ChatShow (SharedValueT s)
     , Eq (SharedValueKeyT s)
     , Eq (SharedValueT s)
     , Eq (SharedEnumerableValueKeyT s)
     , Binary (ServerViewContentT s)
     , Binary (ClientEventT s)
     , Binary (ServerEventT s)
     , Binary (ConnectIdT s)
     , Binary (SharedValueKeyT s)
     , Binary (SharedValueT s)
     , Binary (SharedEnumerableValueKeyT s)
     , NFData s
     , NFData (ServerViewContentT s)
     , NFData (ClientViewT s)
     , NFData (SharedValueKeyT s)
     , NFData (SharedValueT s)
     , NFData (SharedEnumerableValueKeyT s)
     , UIInstructions (ServerViewContentT s)
     , Categorized (ServerEventT s)
     )
 =>
  Server s
 where

  type SharedValueKeyT s
  type SharedValueT s
  type SharedEnumerableValueKeyT s

  -------------- [Server <--> Client] Events -----------------------------------
  type ServerEventT s = (r :: *) | r -> s
  -- ^ Events sent by the server that must be handled by the client.
  type ClientEventT s = (r :: *)| r -> s
  -- ^ Events sent by the client that must be handled by the server.
  type ConnectIdT s = (r :: *) | r -> s
  -- ^ Data passed in 'ClientEvent' 'Connect'.

  --------------- Client, as viewed by the server ------------------------------
  -- | "Server-side" client definition.
  type ClientViewT s = (r :: *) | r -> s
  -- | Some data used when a client is reconnecting.
  type ReconnectionContext s

  ---------------- Server, as viewed by the client -----------------------------
  type ServerViewParamT s = (r :: *) | r -> s
  type ServerViewContentT s = (r :: *) | r -> s

  -- | Called to create the server.
  mkInitial :: (MonadIO m) => ServerViewParamT s -> m (ServerViewContentT s, s)

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
                   -> m (ClientViewT s, ClientName, Color8 Foreground)

  -- | These events are sent to the newly added client.
  greetNewcomer :: (MonadIO m, MonadState (ServerState s) m)
                => m [ServerEventT s]

  -- | For reconnection scenario : called once, only if 'tryReconnect' returned a 'Just'.
  onReconnection :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                 => ReconnectionContext s -> m ()

  onDo :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
       => ServerCommand s -> m ()

  onReport :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
           => ServerReport s -> m ()

  -- | Handle an incoming client event.
  handleClientEvent :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                    => ClientEventT s -> m ()

  -- | Called after a client has been disconnected (either intentionally or on connection error).
  afterClientLeft :: (MonadIO m, MonadState (ServerState s) m)
                  => ClientId
                  -> DisconnectReason -> m ()

class ChatShow a where
  -- | Returns the 'String' to display in the chat window.
  chatShow :: a -> String

data ServerState s = ServerState {
    serverLogs :: {-unpack sum-} !ServerLogs
  , clientsViews :: {-# UNPACK #-} !(ClientViews (ClientViewT s))
  , shouldTerminate :: {-unpack sum-} !Bool
  -- ^ Set on server shutdown
  , content :: !(ServerViewContentT s)
  , unServerState :: !s
} deriving(Generic)
instance (Server s) => NFData (ServerState s)

-- | Commands initiated by a client, executed by the server.
data ServerCommand s =
    Put !(SharedValueT s)
  | Succ !(SharedEnumerableValueKeyT s)
  | Pred !(SharedEnumerableValueKeyT s)
  deriving(Generic) -- Eq needed for parse tests
instance Server s => Binary (ServerCommand s)
instance Server s => Show (ServerCommand s) where
  show (Put a) = show ("Put",a)
  show (Succ a) = show ("Succ",a)
  show (Pred a) = show ("Pred",a)
instance (Server s) => Eq (ServerCommand s) where
  (Put a) == (Put b) = a == b
  (Succ a) == (Succ b) = a == b
  (Pred a) == (Pred b) = a == b
  _ == _ = False
instance (Server s) => ChatShow (ServerCommand s) where
  chatShow (Put x) = chatShow x
  chatShow (Succ x) =
    unwords ["incremented", show x]
  chatShow (Pred x) =
    unwords ["decremented", show x]

-- | Describes what the client wants to know about the server.
data ServerReport s =
    Get !(SharedValueKeyT s)
  deriving(Generic)
instance Server s => Binary (ServerReport s)
instance Server s => Show (ServerReport s) where
  show (Get a) = show ("Get",a)
instance Server s => Eq (ServerReport s) where -- Eq needed for parse tests
  (Get a) == (Get b) = a == b
