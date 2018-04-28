{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Network.Class.ClientNode
    ( ClientNode(..)
    ) where

import           Imj.Prelude

import           Control.Concurrent.Async(Async, wait)
import           Control.Concurrent.STM(TQueue, atomically, writeTQueue)
import           Control.Monad.IO.Class(MonadIO, liftIO)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Types

class (ClientServer (ClientServerT a)) => ClientNode a where
  type ClientServerT a
  sendToServer' :: (MonadIO m
                 , ClientServer (ClientServerT a))
                => a -> ClientEvent (ClientServerT a) -> m ()
  writeToClient' :: (MonadIO m
                 , ClientServer (ClientServerT a))
                 => a -> EventsForClient (ClientServerT a) -> m ()
  serverQueue :: ClientServer (ClientServerT a)
              => a -> TQueue (EventsForClient (ClientServerT a))
-- TODO, with an instance on MVar 
-- class AsyncGroups a
  -- | Attaches an 'Async' to the request, detaches it when the Async is done.
  belongsTo' :: (MonadIO m) => a -> Async () -> WorldId -> m ()
  -- | Cancels every 'Async' currently attached to the request
  cancel' :: (MonadIO m) => a -> WorldId -> m ()


instance (ClientServer s) => ClientNode (ClientQueues s) where
  type ClientServerT (ClientQueues s) = s
  sendToServer' q = liftIO . atomically . writeTQueue (outputQueue q)
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
