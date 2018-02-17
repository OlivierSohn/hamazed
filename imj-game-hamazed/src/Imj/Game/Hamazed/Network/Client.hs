{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Network.Client
      ( appCli
      ) where

import           Imj.Prelude

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM(atomically, writeTQueue, readTQueue)
import           Control.Exception (try)
import           Network.WebSockets(ClientApp, ConnectionException(..), receiveData, sendBinaryData)

import           Imj.Game.Hamazed.Network.Types


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
  safeForever = handleConnectionException . forever
  handleConnectionException :: IO () -> IO ()
  handleConnectionException x =
    try x >>= either
      (\(e :: ConnectionException) -> putStrLn $ "Info|Client disconnection due to:" ++ show e)
      return
