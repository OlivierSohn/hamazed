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
import           Control.Concurrent.STM(writeTQueue, atomically)

import           Imj.Game.Class
import           Imj.Server.Types


-- | Enables asynchronous client server communication.
data ClientQueues g = ClientQueues {
    inputQueue :: {-# UNPACK #-} !(TQueue (EventForClient g))
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
