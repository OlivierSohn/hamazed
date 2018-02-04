{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Server
      ( ClientNode(..)
      , ServerEvtChan(..)
      -- * reexports
      , MonadProcess
      ) where

import           Imj.Prelude
import           Control.Distributed.Process.Lifted(SendPort, ReceivePort)
import           Control.Distributed.Process.Lifted.Class(MonadProcess)

import           Imj.Game.Hamazed.Loop.Event.Types

-- | A 'ClientNode' sends 'ClientEvent's and receives 'ServerEvent's.
class ClientNode a where
  send :: (MonadProcess m) => a -> ClientEvent -> m ()
  tryReceive :: (MonadProcess m) => a -> m (Maybe ServerEvent)

data ServerEvtChan = ServerEvtChan !(SendPort ServerEvent) !(ReceivePort ServerEvent)
