{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Network.GameNode
      ( startNetworking
      ) where

import           Imj.Prelude
import           Control.Concurrent.STM(newTQueueIO)
import           Control.Concurrent (forkIO, newMVar, forkIOWithUnmask)
import           Control.Exception (onException, finally, bracket, allowInterrupt, mask_)
import           Network.Socket(withSocketsDo, Socket, close, accept)
import           Network.WebSockets(defaultConnectionOptions, makeListenSocket, makePendingConnection,
                    ServerApp, ConnectionOptions, runClient)
import           Network.WebSockets.Connection(PendingConnection(..))
import qualified Network.WebSockets.Stream as Stream(close)

import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Network.Class.ClientNode
import           Imj.Game.Hamazed.Network.Client(appCli)
import           Imj.Game.Hamazed.Network.Server(appSrv, gameScheduler)

startNetworking :: SuggestedPlayerName -> Server -> IO ClientQueues
startNetworking playerName srv = withSocketsDo $ do
  -- start server
  start $ GameServer srv
  -- by now, if the server is local, the listening socket has been created.
  qs <- mkQueues
  -- start client
  start $ GameClient qs srv
  -- initiate the game connection
  sendToServer' qs $
    Connect playerName $
      case srv of
        (Local _) -> WorldCreator
        _ -> JustPlayer
  return qs

mkQueues :: IO ClientQueues
mkQueues =
  ClientQueues <$> newTQueueIO <*> newTQueueIO

start :: GameNode -> IO ()
start n = case n of
  (GameServer (Distant _ _)) -> return ()
  (GameServer srv@(Local _)) -> do
    let (ServerName host, ServerPort port) = getServerNameAndPort srv
    listenSock <- flip onException failure $
      makeListenSocket host port -- TODO should we close it, and when?
    success
    state <- newMVar newServerState
    -- 1 thread to listen for incomming connections, n thread to handle clients
    _ <- forkIO $ runServer' listenSock $ appSrv state
    -- 1 thread to periodically send game events
    _ <- forkIO $ gameScheduler state
    return ()
  (GameClient q s) -> do
    let (ServerName name, ServerPort port) = getServerNameAndPort s
    _ <- forkIO $ withSocketsDo $ flip onException failure $
      runClient name port "/" $ \x -> do
        success
        appCli q x
    return ()
 where
  success = putStrLn $ "Success: " ++ msg n True
  failure = putStrLn $ "Error: "   ++ msg n False
  msg (GameServer s) x = "Hamazed GameServer " ++ st x ++ show s ++ ")"
   where
    st False = "failed to start ("
    st True = "started ("
  msg (GameClient _ s) x = "Hamazed GameClient " ++ st x ++ " to Hamazed GameServer (" ++ show s ++ ")"
   where
    st False = "failed to connect"
    st True = "connected"

-- Adapted from https://hackage.haskell.org/package/websockets-0.12.3.1/docs/src/Network-WebSockets-Server.html#runServer
-- (I needed to know when the listening socket was ready)
runServer' :: Socket -- ^ Listening socket
           -> ServerApp  -- ^ Application
           -> IO ()      -- ^ Never returns
runServer' listeningSocket app =
  runServerWith' listeningSocket defaultConnectionOptions app


-- Adapted from https://hackage.haskell.org/package/websockets-0.12.3.1/docs/src/Network-WebSockets-Server.html#runServer
runServerWith' :: Socket -> ConnectionOptions -> ServerApp -> IO ()
runServerWith' listeningSock opts app =
  mask_ $ forever $ do
    allowInterrupt
    (conn, _) <- accept listeningSock
    void $ forkIOWithUnmask $ \unmask ->
      flip finally (close conn) (unmask $ runApp conn opts app)

-- Copied from https://hackage.haskell.org/package/websockets-0.12.3.1/docs/src/Network-WebSockets-Server.html#runServer
runApp :: Socket
       -> ConnectionOptions
       -> ServerApp
       -> IO ()
runApp socket opts app =
  bracket
    (makePendingConnection socket opts)
    (Stream.close . pendingStream)
    app
