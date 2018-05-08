{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.ServerView.Types
      ( ServerView(..)
      , ServerType(..)
      , ServerName(..)
      , ServerPort(..)
      , ServerContent(..)
      , ConnectionStatus(..)
      ) where

import           Imj.Prelude
import           Data.String(IsString)

import           Imj.ClientView.Internal.Types
import           Imj.Server.Internal.Types
import           Imj.Server.Color

data ConnectionStatus =
    NotConnected
  | Connected {-# UNPACK #-} !ClientId
  | ConnectionFailed {-# UNPACK #-} !Text

data ServerView values = ServerView {
    serverType :: !ServerType
  , serverContent :: !(ServerContent values)
}  deriving(Generic, Show)

data ServerContent cached = ServerContent {
    serverPort :: {-# UNPACK #-} !ServerPort
  , cachedValues :: !(Maybe cached)
    -- ^ To avoid querying the server when we know that the content didn't change.
}  deriving(Generic, Show)


data ServerType =
    Distant !ServerName
  | Local !ServerLogs !(Maybe ColorScheme)
  deriving(Generic, Show)

newtype ServerName = ServerName String
  deriving (Show, IsString, Eq)

newtype ServerPort = ServerPort Int
  deriving (Generic, Show, Num, Integral, Real, Ord, Eq, Enum)
