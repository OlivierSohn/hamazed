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
      , Value(..)
      , ValueKey(..)
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
      , gets'
      -- * reexports
      , MonadReader
      , MonadState
      ) where

import           Imj.Prelude
import           Data.Proxy(Proxy)
import           Data.List(unwords)
import           Data.String(IsString(..))
import           Control.Concurrent.MVar.Strict (MVar)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad.State.Strict(MonadState, gets)

import           Imj.Arg.Class
import           Imj.Categorized
import           Imj.ClientView.Internal.Types
import           Imj.Graphics.Class.UIInstructions
import           Imj.Graphics.Color.Types
import           Imj.Server.Internal.Types

import           Imj.Network

class (Show (ClientEventT s)
     , Show (ConnectIdT s)
     , Show (ValuesT s)
     , Show (ClientViewT s)
     , Show (ValueKeyT s)
     , Show (ValueT s)
     , Show (EnumValueKeyT s)
     , Show (StateValueT s)
     , ChatShow (ValueT s)
     , Arg (ConnectIdT s)
     , ClientNameSuggestion (ConnectIdT s)
     , Eq (ValueKeyT s)
     , Eq (ValueT s)
     , Eq (EnumValueKeyT s)
     , Binary (ValuesT s)
     , Binary (ClientEventT s)
     , Binary (ServerEventT s)
     , Binary (ConnectIdT s)
     , Binary (ValueKeyT s)
     , Binary (ValueT s)
     , Binary (EnumValueKeyT s)
     , Binary (StateValueT s)
     , NFData s -- because we use Control.Concurrent.MVar.Strict
     , NFData (ValuesT s)
     , NFData (ClientViewT s)
     , NFData (ValueKeyT s)
     , NFData (ValueT s)
     , NFData (EnumValueKeyT s)
     , UIInstructions (ValuesT s)
     , Categorized (ServerEventT s)
     )
 =>
  Server s
 where

  type ValueKeyT s
  type ValueT s
  type EnumValueKeyT s

  type ValuesT s = (r :: *) | r -> s

  type ServerEventT s = (r :: *) | r -> s
  -- ^ Events sent by the server that must be handled by the client.
  type ClientEventT s = (r :: *)| r -> s
  -- ^ Events sent by the client that must be handled by the server.
  type ConnectIdT s = (r :: *) | r -> s
  -- ^ Data passed in 'ClientEvent' 'Connect'.

  -- | "Server-side" client definition.
  type ClientViewT s = (r :: *) | r -> s
  -- | Some data used when a client is reconnecting.
  type ReconnectionContext s

  type StateValueT s

  -- | Called to create the server.
  mkInitial :: MonadIO m => Proxy s -> m (ValuesT s, s)

  -- | Returns actions that are not associated to a particular client, and that
  -- need to be run as long as the server is running. For a game server,
  -- the list will typically contain a game scheduling action.
  inParallel :: [MVar (ServerState s) -> IO ()]
  inParallel = []

  -- | When returning Left, the corresponding client connection is rejected.
  acceptConnection :: Maybe (ConnectIdT s) -> Either Text ()

  -- | Creates the client view
  mkInitialClient :: ClientViewT s

  mkClientName :: Maybe (ConnectIdT s) -> ClientName Proposed
  mkClientName = maybe (fromString "Player") extractName

  -- | These events are sent to the newly added client.
  greetNewcomer :: (MonadIO m, MonadState (ServerState s) m)
                => m [ServerEventT s]
  greetNewcomer = return []

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

  -- NOTE the signatures of getValue / onPut / onDelta will be unified later on
  getValue :: ValueKeyT s
           -> ValuesT s
           -> ValueT s
  onPut :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
        => ValueT s
        -> m ()
  onDelta :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
          => Int
          -> EnumValueKeyT s
          -> m ()

  -- | Handle an incoming client event.
  handleClientEvent :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                    => ClientEventT s -> m ()

  -- | Returns True if the client was included in the game that is being setup.
  clientCanJoin :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                => Proxy s -> m Bool
  clientCanJoin _ = return True

  -- | Returns True if the client starts the game.
  clientCanTransition :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                      => StateValueT s -> m Bool
  clientCanTransition _ = return True

  -- | Called after a client has disconnected (either intentionally or on connection error).
  --
  -- Default implementation does nothing.
  afterClientLeft :: (MonadIO m, MonadState (ServerState s) m)
                  => ClientId
                  -> m ()
  afterClientLeft _ = return ()

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
  , content :: !(ValuesT s)
  , centerColor :: {-# UNPACK #-} !(Color8 Foreground)
  -- ^ The color scheme.
  , unServerState :: !s
} deriving(Generic)
instance (Server s) => NFData (ServerState s)

{-# INLINABLE gets' #-}
gets' :: (MonadState (ServerState s) m)
      => (s -> a)
      -> m a
gets' f = gets (f . unServerState)

-- | Commands initiated by a client, executed by the server.
data ServerCommand s =
    Put !(Value s)
  | Succ !(EnumValueKeyT s)
  | Pred !(EnumValueKeyT s)
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
    Get !(ValueKey s)
  deriving(Generic)
instance Server s => Binary (ServerReport s)
instance Server s => Show (ServerReport s) where
  show (Get a) = show ("Get",a)
instance Server s => Eq (ServerReport s) where -- Eq needed for parse tests
  (Get a) == (Get b) = a == b

-- | Value shared by all clients.
data Value s =
    ColorSchemeCenter {-# UNPACK #-} !(Color8 Foreground)
  | AppValue !(ValueT s)
  deriving(Generic) -- Eq needed for parse tests
instance Server s => Eq (Value s) where
  ColorSchemeCenter x == ColorSchemeCenter y = x == y
  AppValue x == AppValue y = x == y
  ColorSchemeCenter _ == AppValue _ = False
  AppValue _ == ColorSchemeCenter _ = False
instance Server s => Show (Value s) where
  show (ColorSchemeCenter x) = show ("ColorSchemeCenter",x)
  show (AppValue x) = show ("AppValue",x)
instance Server s => Binary (Value s)
instance Server s => NFData (Value s)
instance Server s => ChatShow (Value s) where
  chatShow (ColorSchemeCenter c) =
    unwords ["color scheme center:", show $ color8CodeToXterm256 c]
  chatShow (AppValue x) =
    chatShow x

-- | Identifiers of values shared by all clients.
data ValueKey s =
    ColorSchemeCenterKey
  | AppValueKey (ValueKeyT s)
  deriving(Generic) -- Eq needed for parse tests
instance Server s => Binary (ValueKey s)
instance Server s => NFData (ValueKey s)
instance Server s => Eq (ValueKey s) where
  ColorSchemeCenterKey == ColorSchemeCenterKey = True
  AppValueKey x == AppValueKey y = x == y
  ColorSchemeCenterKey == AppValueKey _ = False
  AppValueKey _ == ColorSchemeCenterKey = False
instance Server s => Show (ValueKey s) where
  show ColorSchemeCenterKey = "ColorSchemeCenterKey"
  show (AppValueKey x) = show ("AppValueKey",x)
