{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Network.Class.ClientNode
    ( ClientNode(..)
    ) where

import           Imj.Prelude

import           Control.Concurrent.Async(Async, wait)
import           Control.Concurrent.STM(TQueue, atomically, writeTQueue)
import           Control.Monad.IO.Class(MonadIO, liftIO)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Types

-- | A 'ClientNode' sends 'ClientEvent's and receives 'ServerEvent's.
class ClientNode a where
  sendToServer' :: (MonadIO m) => a -> ClientEvent -> m ()
  writeToClient' :: (MonadIO m) => a -> EventsForClient -> m ()
  serverQueue :: a -> TQueue EventsForClient

  -- | Attaches an 'Async' to the request, detaches it when the Async is done.
  belongsTo' :: (MonadIO m) => a -> Async () -> WorldId -> m ()
  -- | Cancels every 'Async's currently attached to the request
  cancel' :: (MonadIO m) => a -> WorldId -> m ()


instance ClientNode ClientQueues where
  sendToServer' q = liftIO .atomically . writeTQueue (outputQueue q)
  writeToClient' q = liftIO . atomically . writeTQueue (inputQueue q)
  serverQueue = inputQueue

  belongsTo' (ClientQueues _ _ m) a w =
    liftIO $ do
      addRequestAsync m a w
      void $ wait a
      removeRequestAsync m a w

  cancel' (ClientQueues _ _ m) = liftIO . releaseRequestResources m

  {-# INLINABLE sendToServer' #-}
  {-# INLINABLE writeToClient' #-}
  {-# INLINABLE serverQueue #-}
  {-# INLINABLE belongsTo' #-}
  {-# INLINABLE cancel' #-}
