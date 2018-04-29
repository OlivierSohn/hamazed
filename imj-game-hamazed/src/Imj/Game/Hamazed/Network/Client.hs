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
import           Data.Text(pack)
import           Network.WebSockets(ClientApp, ConnectionException(..), receiveData, sendBinaryData)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Loop.Event.Types

import           Imj.Client.Types
import           Imj.Client.Class
import           Imj.Client

-- TODO split Event between what is generic ('Log') and the rest, like we did for other events.
appCli :: Server s => ClientQueues Event s -> ClientApp ()
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
        atomically . writeTQueue toClient $ FromClient $ Log Info $ "Client disconnects:" <> pack (show e))
      return
