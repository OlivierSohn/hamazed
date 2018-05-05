{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.ClientView.Internal.Types
      ( ClientViews(..)
      , ClientView(..)
      , ConstClientView(..)
      , ClientId(..)
      , ClientStatus(..)
      , ClientEssence(..)
      , ServerOwnership(..)
      ) where

import           Imj.Prelude
import           Data.Int(Int64)
import           Data.Map.Strict(Map)
import           Network.WebSockets(Connection)

import           Imj.Network
import           Imj.Graphics.Color

-- | Immutable data associated to a client.
data ConstClientView = ConstClientView {
    connection :: {-# UNPACK #-} !Connection
  , clientId :: {-# UNPACK #-} !ClientId
}

data ClientViews c = ClientViews {
    views :: !(Map ClientId (ClientView c))
    -- ^ Only connected clients are here: once a client is disconnected, it is removed from the Map.
  , getNextClientId :: !ClientId
    -- ^ The 'ClientId' that will be assigned to the next new client.
} deriving(Generic)
instance (NFData c) => NFData (ClientViews c)

newtype ClientId = ClientId Int64
  deriving(Generic, Binary, Eq, Ord, Show, Enum, NFData, Integral, Real, Num)

data ClientView c = ClientView {
    getConnection :: {-# UNPACK #-} !Connection
  , getServerOwnership :: {-unpack sum-} !ServerOwnership
  , getName :: {-# UNPACK #-} !(ClientName Approved)
  , getColor :: {-# UNPACK #-} !(Color8 Foreground)
  , unClientView :: !c
} deriving(Generic)
instance NFData c => NFData (ClientView c) where
  rnf (ClientView _ a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
instance Show c => Show (ClientView c) where
  show (ClientView _ a b c d ) = show ("ClientView" :: String,a,b,c,d)
instance Functor ClientView where
  {-# INLINE fmap #-}
  fmap f c = c { unClientView = f $ unClientView c}

data ServerOwnership =
    ClientOwnsServer
    -- ^ Implies that if the client is shutdown, the server is shutdown too.
  | ClientDoesntOwnServer
  deriving(Generic, Show, Eq)
instance Binary ServerOwnership
instance NFData ServerOwnership

data ClientEssence = ClientEssence {
    clientEssenceName :: {-# UNPACK #-} !(ClientName Approved)
  , clientEssenceStatus :: {-unpack sum-} !ClientStatus
  , clientEssenceColor :: {-# UNPACK #-} !(Color8 Foreground)
} deriving(Generic, Show)
instance Binary ClientEssence

data ClientStatus = Present | Absent
  deriving(Generic, Show)
instance Binary ClientStatus
