{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Network.Class.ClientNode
    ( ClientNode(..)
    , sendToServer
    ) where

import           Imj.Prelude

import           Control.Concurrent.STM(TQueue, atomically, writeTQueue)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Reader.Class(MonadReader, asks)

import           Imj.Game.Hamazed.Network.Types

-- | A 'ClientNode' sends 'ClientEvent's and receives 'ServerEvent's.
class ClientNode a where
  sendToServer' :: (MonadIO m) => a -> ClientEvent -> m ()
  serverQueue :: a -> TQueue ServerEvent

instance ClientNode ClientQueues where
  sendToServer' q = liftIO . atomically . writeTQueue (getOutputQueue q)
  serverQueue = getInputQueue
  {-# INLINABLE sendToServer' #-}
  {-# INLINABLE serverQueue #-}

{-# INLINABLE sendToServer #-}
sendToServer :: (MonadReader e m, ClientNode e, MonadIO m) => ClientEvent -> m ()
sendToServer e = asks sendToServer' >>= \f -> f e
