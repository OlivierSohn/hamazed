{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.ServerView.Types
      ( ServerView(..)
      , ServerType(..)
      , ServerName(..)
      , ServerContent(..)
      , ConnectionStatus(..)
      ) where

import           Imj.Prelude
import           Data.String(IsString)

import           Imj.ClientView.Internal.Types
import           Imj.Network

data ConnectionStatus =
    NotConnected
  | Connected {-# UNPACK #-} !ClientId
  | ConnectionFailed {-# UNPACK #-} !Text

data ServerContent cached = ServerContent {
    serverPort :: {-# UNPACK #-} !ServerPort
  , cachedValues :: !(Maybe cached)
    -- ^ To avoid querying the server when we know that the content didn't change.
}  deriving(Generic, Show)


data ServerView values = ServerView {
    serverType :: !ServerType
  , serverContent :: !(ServerContent values)
}  deriving(Generic, Show)

data ServerType =
    Distant !ServerName
  | Local
  deriving(Generic, Show)

newtype ServerName = ServerName String
  deriving (Show, IsString, Eq)
