{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Network.Client
      ( appCli
      ) where

import           Imj.Prelude hiding (drop, null, intercalate)

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM(atomically, writeTQueue, readTQueue)
import           Network.WebSockets(ClientApp, receiveData, sendBinaryData)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Network.Internal.Types


appCli :: ClientQueues -> ClientApp ()
appCli (ClientQueues fromServer toServer) conn = do
  void $ forkIO $
    safeForever $
      receiveData conn
        >>= liftIO . atomically . writeTQueue fromServer
  safeForever $
    liftIO (atomically (readTQueue toServer))
      >>= sendBinaryData conn
 where
  safeForever = handleConnectionException "Client" . forever
