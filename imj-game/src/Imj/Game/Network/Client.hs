{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Network.Client
      ( appCli
      ) where

import           Imj.Prelude

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM(atomically, writeTQueue, readTQueue)
import           Control.Exception (try)
import           Data.Text(pack)
import           Network.WebSockets(ClientApp, ConnectionException(..), receiveData, sendBinaryData)

import           Imj.Game.Class

import           Imj.Event
import           Imj.Game.Network.ClientQueues
import           Imj.Log

appCli :: GameLogic g
       => ClientQueues g
       -> ClientApp ()
appCli q@(ClientQueues toClient toServer) conn = do
  void $ forkIO $
    safeForever $
      receiveData conn >>= writeToClient' q . FromServer
  safeForever $
    liftIO (atomically $ readTQueue toServer) >>= sendBinaryData conn
 where
  safeForever = handleConnectionException . forever
  handleConnectionException :: IO () -> IO ()
  handleConnectionException x =
    try x >>= either
      (\(e :: ConnectionException) ->
        -- Maybe noone is reading at the end of the queue if the client already disconnected.
        -- That's ok.
        atomically . writeTQueue toClient $ FromClient $ (Log Info $ "Client disconnects:" <> pack (show e)))
      return
