{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Network.Class.ClientNode
    ( ClientNode(..)
    ) where

import           Imj.Prelude

import           Control.Concurrent.STM(TQueue, atomically, writeTQueue)
import           Control.Monad.IO.Class(MonadIO, liftIO)

import           Imj.Game.Hamazed.Network.Types

-- | A 'ClientNode' sends 'ClientEvent's and receives 'ServerEvent's.
class ClientNode a where
  sendToServer' :: (MonadIO m) => a -> ClientEvent -> m ()
  writeToClient' :: (MonadIO m) => a -> EventsForClient -> m ()
  serverQueue :: a -> TQueue EventsForClient

instance ClientNode ClientQueues where
  sendToServer' q = liftIO .atomically . writeTQueue (getOutputQueue q)
  writeToClient' q = liftIO . atomically . writeTQueue (getInputQueue q)
  serverQueue = getInputQueue
  {-# INLINABLE sendToServer' #-}
  {-# INLINABLE serverQueue #-}
