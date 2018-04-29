{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Client
      ( ClientQueues(..)
      ) where

import           Imj.Prelude
import           Control.Concurrent.STM(TQueue, atomically, writeTQueue)

import           Imj.Client.Class
import           Imj.ClientServer.Types

-- | Allows the client to communicate with the server asynchronously.
data ClientQueues c s = ClientQueues {
    inputQueue :: {-# UNPACK #-} !(TQueue (EventsForClient c s))
  , outputQueue :: {-# UNPACK #-} !(TQueue (ClientEvent s))
}

instance (ClientServer s) => ClientNode (ClientQueues c s) where
  type ClientServerT (ClientQueues c s) = s
  type CliEvtT (ClientQueues c s) = c

  sendToServer' q = liftIO . atomically . writeTQueue (outputQueue q)
  writeToClient' q = liftIO . atomically . writeTQueue (inputQueue q)
  serverQueue = inputQueue

  {-# INLINABLE sendToServer' #-}
  {-# INLINABLE writeToClient' #-}
  {-# INLINABLE serverQueue #-}
