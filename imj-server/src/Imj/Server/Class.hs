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
import           Data.String(IsString)
import           Control.Concurrent.MVar.Strict (MVar)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad.State.Strict(MonadState)

import           Imj.Arg.Class
import           Imj.Categorized
import           Imj.ClientView.Internal.Types
import           Imj.Graphics.Class.UIInstructions
import           Imj.Server.Internal.Types

import           Imj.Graphics.Color
import           Imj.Network

class (Show (ClientEventT s)
     , Show (ConnectIdT s)
     , Show (ServerConfigT s)
     , Show (ServerContentT s)
     , Show (ClientViewT s)
     , Show (SharedValueKeyT s)
     , Show (SharedValueT s)
     , Show (SharedEnumerableValueKeyT s)
     , ChatShow (SharedValueT s)
     , Arg (ServerConfigT s)
     , Eq (SharedValueKeyT s)
     , Eq (SharedValueT s)
     , Eq (SharedEnumerableValueKeyT s)
     , IsString (ConnectIdT s)
     , Binary (ServerContentT s)
     , Binary (ClientEventT s)
     , Binary (ServerEventT s)
     , Binary (ConnectIdT s)
     , Binary (SharedValueKeyT s)
     , Binary (SharedValueT s)
     , Binary (SharedEnumerableValueKeyT s)
     , NFData s -- because we use Control.Concurrent.MVar.Strict
     , NFData (ServerContentT s)
     , NFData (ClientViewT s)
     , NFData (SharedValueKeyT s)
     , NFData (SharedValueT s)
     , NFData (SharedEnumerableValueKeyT s)
     , UIInstructions (ServerContentT s)
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
  type ServerConfigT s = (r :: *) | r -> s
  type ServerContentT s = (r :: *) | r -> s

  -- | Called to create the server.
  mkInitial :: (MonadIO m) => Maybe (ServerConfigT s) -> m (ServerContentT s, s)

  -- | Returns actions that are not associated to a particular client, and that
  -- need to be run as long as the server is running. For a game server,
  -- the list will typically contain a game scheduling action.
  inParallel :: [MVar (ServerState s) -> IO ()]
  inParallel = []

  -- | When returning Left, the corresponding client connection is rejected.
  acceptConnection :: Maybe (ConnectIdT s) -> Either Text ()

  -- | Creates the client view, given the 'ClientId' and 'ConnectIdT'.
  createClientView :: (MonadIO m, MonadState (ServerState s) m)
                   => ClientId
                   -> Maybe (ConnectIdT s)
                   -> m (ClientViewT s, ClientName, Color8 Foreground)

  -- | These events are sent to the newly added client.
  greetNewcomer :: (MonadIO m, MonadState (ServerState s) m)
                => m [ServerEventT s]

  -- | Return 'Just' if the client identified by its 'ConnectIdT' should be considered reconnecting.
  --
  -- The default implementation returns 'Nothing'.
  tryReconnect :: (MonadIO m, MonadState (ServerState s) m)
               => Maybe (ConnectIdT s)
               -> m (Maybe (ClientId, ReconnectionContext s))
  tryReconnect _ = return Nothing

  -- | For reconnection scenario : called once, only if 'tryReconnect' returned a 'Just'.
  --
  -- Default implementation does nothing.
  onReconnection :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                 => ReconnectionContext s -> m ()
  onReconnection _ = return ()

  onDo :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
       => ServerCommand s -> m ()

  onReport :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
           => ServerReport s -> m ()

  -- | Handle an incoming client event.
  handleClientEvent :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                    => ClientEventT s -> m ()

  -- | Called after a client has been disconnected (either intentionally or on connection error).
  --
  -- Default implementation does nothing.
  afterClientLeft :: (MonadIO m, MonadState (ServerState s) m)
                  => ClientId
                  -> DisconnectReason -> m ()
  afterClientLeft _ _ = return ()

class Show a => ChatShow a where
  -- | Returns the 'String' to display in the chat window. The default implemention
  -- is 'show'.
  chatShow :: a -> String
  chatShow = show

instance ChatShow ()

data ServerState s = ServerState {
    serverLogs :: {-unpack sum-} !ServerLogs
  , clientsViews :: {-# UNPACK #-} !(ClientViews (ClientViewT s))
  , shouldTerminate :: {-unpack sum-} !Bool
  -- ^ Set on server shutdown
  , content :: !(ServerContentT s)
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
