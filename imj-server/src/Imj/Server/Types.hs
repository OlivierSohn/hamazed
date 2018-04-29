{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Server.Types
      ( -- * Server-side
        {- | Types used by a server to represent itself. -}
        ClientId
      , Clients
      , Client, mkClient, unClient
      , ServerLogs(..)
      , ServerOwnership(..)
      , DisconnectReason(..)
      , ServerState, unServerState, clientsMap
      , mkServerState
      , ClientServer(..)
      , mapState
        -- * Client-side
        {- | Types used by a client to represent a server, and the connection to the server. -}
      , Server(..)
      , ServerType(..)
      , ServerName(..)
      , ServerPort(..)
      , ServerContent(..)
      , ConnectionStatus(..)
      ) where

import           Imj.Prelude
import qualified Data.Map.Strict as Map

import           Imj.ClientServer.Class
import           Imj.Server.Internal.Types

--------------------------------------------------------------------------------
---------- Seen from a server's perspective ------------------------------------
--------------------------------------------------------------------------------

mkEmptyClients :: Clients c
mkEmptyClients = Clients Map.empty (ClientId 0)

mkServerState :: ServerLogs -> s -> ServerState s
mkServerState logs s =
  ServerState logs mkEmptyClients False s


--------------------------------------------------------------------------------
---------- Seen from a client's perspective ------------------------------------
--------------------------------------------------------------------------------

data ConnectionStatus =
    NotConnected
  | Connected {-# UNPACK #-} !ClientId
  | ConnectionFailed {-# UNPACK #-} !Text

data Server param cached = Server {
    serverType :: !(ServerType param)
  , serverContent :: !(ServerContent cached)
}  deriving(Generic, Show)

data ServerContent cached = ServerContent {
    serverPort :: {-# UNPACK #-} !ServerPort
  , cachedContent :: !(Maybe cached)
    -- ^ To avoid querying the server when we know that the content didn't change.
}  deriving(Generic, Show)
