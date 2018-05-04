{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Network.ClientQueues
      ( ClientQueues(..)
      ) where

import           Imj.Prelude
import           Control.Concurrent.STM(TQueue, writeTQueue, atomically)
import           Control.Monad.IO.Class(liftIO)

import           Imj.Game.Types
import           Imj.Server.Types


-- | Allows the client to communicate with the server asynchronously.
data ClientQueues g = ClientQueues {
    inputQueue :: {-# UNPACK #-} !(TQueue (EventsForClient g))
  , outputQueue :: {-# UNPACK #-} !(TQueue (ClientEvent (ServerT g)))
}

instance (GameLogic g) => Client (ClientQueues g) where
  type GameLogicT (ClientQueues g) = g

  sendToServer' q = liftIO . atomically . writeTQueue (outputQueue q)
  writeToClient' q = liftIO . atomically . writeTQueue (inputQueue q)
  serverQueue = inputQueue

  {-# INLINABLE sendToServer' #-}
  {-# INLINABLE writeToClient' #-}
  {-# INLINABLE serverQueue #-}
