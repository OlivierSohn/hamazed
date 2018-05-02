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
import           Imj.Server.Class

data ConnectionStatus =
    NotConnected
  | Connected {-# UNPACK #-} !ClientId
  | ConnectionFailed {-# UNPACK #-} !Text

data ServerView s = ServerView {
    serverType :: !(ServerType (ServerViewParamT s))
  , serverContent :: !(ServerContent (ServerViewContentT s))
}  deriving(Generic)
instance Server s => Show (ServerView s) where
  show (ServerView t c) = show ("ServerView",t,c)

data ServerContent cached = ServerContent {
    serverPort :: {-# UNPACK #-} !ServerPort
  , cachedContent :: !(Maybe cached)
    -- ^ To avoid querying the server when we know that the content didn't change.
}  deriving(Generic, Show)


data ServerType p =
    Distant !ServerName
  | Local !ServerLogs !p
  deriving(Generic, Show)

newtype ServerName = ServerName String
  deriving (Show, IsString, Eq)

newtype ServerPort = ServerPort Int
  deriving (Generic, Show, Num, Integral, Real, Ord, Eq, Enum)
