{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.ClientView.Internal.Types
      ( ClientViews(..)
      , ClientView(..)
      , ConstClientView(..)
      , ClientId(..)
      , ServerOwnership(..)
      ) where

import           Imj.Prelude
import           Data.Int(Int64)
import           Data.Map.Strict(Map)
import           Network.WebSockets(Connection)


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
  , unClientView :: !c
} deriving(Generic)
instance NFData c => NFData (ClientView c) where
  rnf (ClientView _ a b) = rnf a `seq` rnf b
instance Show c => Show (ClientView c) where
  show (ClientView _ a b) = show ("ClientView" :: String,a,b)
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
