{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Network.Client
      ( appCli
      ) where

import           Imj.Prelude hiding (drop, null, intercalate)

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM(atomically, writeTQueue, readTQueue)
import           Network.WebSockets(ClientApp, receiveData, sendBinaryData)

import           Imj.Game.Hamazed.Network.Types

appCli :: ClientQueues -> ClientApp ()
appCli (ClientQueues fromServer toServer) conn = do
  -- continuously receive objects from the websocket and writes to the input queue.
  void $ forkIO $ forever $
    receiveData conn >>= liftIO . atomically . writeTQueue fromServer
  -- continuously read from the output queue and send to the server.
  forever $
    liftIO (atomically (readTQueue toServer)) >>= sendBinaryData conn
