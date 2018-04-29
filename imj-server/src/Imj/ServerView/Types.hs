{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

import           Imj.Server.Class


data ConnectionStatus =
    NotConnected
  | Connected {-# UNPACK #-} !ClientId
  | ConnectionFailed {-# UNPACK #-} !Text

data ServerView param cached = ServerView {
    serverType :: !(ServerType param)
  , serverContent :: !(ServerContent cached)
}  deriving(Generic, Show)

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
