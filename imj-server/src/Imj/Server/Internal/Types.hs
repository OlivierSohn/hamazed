{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Server.Internal.Types
      ( ServerLogs(..)
      , DisconnectReason(..)
      , ClientLifecycle(..)
      ) where

import           Imj.Prelude
import           Data.Text(unpack)

data ClientLifecycle =
    NewClient
  | ReconnectingClient
  deriving(Show)

data ServerLogs =
    NoLogs
  | ConsoleLogs
  deriving(Generic, Show)
instance NFData ServerLogs

data DisconnectReason =
    ClientShutdown !(Either Text ())
    -- ^ A client is disconnected because {'Right' : it decided so, 'Left' : its connection became unusable}.
  | ServerShutdown {-# UNPACK #-} !Text
  -- ^ All clients are disconnected.
  deriving(Generic)
instance Binary DisconnectReason
instance Show DisconnectReason where
  show (ServerShutdown t) = unpack $ "Server shutdown < " <> t
  show (ClientShutdown (Right ()))  = "Client shutdown"
  show (ClientShutdown (Left t)) = unpack $ "Broken client < " <> t
