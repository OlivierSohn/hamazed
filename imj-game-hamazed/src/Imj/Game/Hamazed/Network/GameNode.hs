{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Hamazed.Network.GameNode
      ( startClient
      , startServerIfLocal
      ) where

import           Imj.Prelude
import           Control.Concurrent(ThreadId, forkIO, myThreadId, throwTo)
import           Control.Concurrent.STM(newTQueueIO)
import           Control.Exception (SomeException, try, onException, finally, bracket)
import           Control.Concurrent.MVar.Strict (MVar, newMVar, putMVar, modifyMVar_)
import           Control.Monad.State.Strict(execStateT)
import           Data.Text(pack)
import           Foreign.C.Types(CInt)
import           Network.Socket(Socket, SocketOption(..), setSocketOption, close, accept)
import           Network.WebSockets(ServerApp, ConnectionOptions, defaultConnectionOptions,
                    makeListenSocket, makePendingConnection, runClient)
import           Network.WebSockets.Connection(PendingConnection(..))
import qualified Network.WebSockets.Stream as Stream(close)
import           System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types
import           Imj.ServerView.Types
import           Imj.ServerView

import           Imj.Event
import           Imj.Client
import           Imj.Game.Hamazed.Network.Client(appCli)
import           Imj.Log
import           Imj.Server.Run

startServerIfLocal :: Server s
                   => ServerView s
                   -> MVar (Either String String)
                   -- ^ Will be set when the client can connect to the server.
                   -> IO ()
startServerIfLocal srv@(ServerView (Distant _) _) v = putMVar v $ Right $ "Client will try to connect to: " ++ show srv
startServerIfLocal srv@(ServerView (Local logs a) _) v = do
  let (ServerName host, ServerPort port) = getServerNameAndPort srv
  listen <- makeListenSocket host port `onException` putMVar v (Left $ msg False)
  putMVar v $ Right $ msg True -- now that the listen socket is created, signal it.
  uncurry (mkServerState logs) <$> mkInitial a >>= newMVar >>= \state -> do
    serverMainThread <- myThreadId
    mapM_ (installOneHandler state serverMainThread)
          [(sigINT,  "sigINT")
         , (sigTERM, "sigTERM")]
    mapM_ (\parallel -> void $ forkIO $ parallel state) inParallel
    -- the current thread listens for incomming connections, 1 thread is forked per client
    runServer' listen $ appSrv state
    -- appSrv :: MVar (ServerState Hamazed) -> PendingConnection -> IO ()
 where
  msg x = "Hamazed GameServer " ++ st x ++ show srv ++ ")"
   where
    st False = "failed to start ("
    st True = "starts listening ("

startClient :: GameLogic g
            => ConnectIdT (ServerT g)
            -> ServerView (ServerT g)
            -> IO (ClientQueues g)
startClient playerName srv = do
  -- by now, if the server is local, the listening socket has been created.
  qs <- mkQueues
  let reportError x = try x >>= either
        (\(e :: SomeException) ->
          -- Maybe noone is reading at the end of the queue if the client already disconnected.
          -- That's ok.
          writeToClient' qs $ FromClient $ Log Error $ msg "Failed to connect" <> ":" <> pack (show e))
        return

  -- start client
  let (ServerName name, ServerPort port) = getServerNameAndPort srv
  _ <- forkIO $
    -- runClient sets the NO_DELAY socket option to 1, so we don't need to do it.
    reportError $ runClient name port "/" $ \x -> do
      writeToClient' qs $ FromClient $ Log Info $ msg "Connected"
      appCli qs x
  -- initialize the game connection
  sendToServer' qs $
    Connect playerName $
      case serverType srv of
        Local {} -> ClientOwnsServer
        Distant {} -> ClientDoesntOwnServer
  return qs
 where
  msg x = x <> " to server " <> pack (show srv)

mkQueues :: IO (ClientQueues s)
mkQueues =
  ClientQueues <$> newTQueueIO <*> newTQueueIO

installOneHandler :: Server s => MVar (ServerState s) -> ThreadId -> (CInt, Text) -> IO ()
installOneHandler state serverThreadId (sig,sigName) =
  void $ installHandler sig (Catch $ handleTermination sigName state serverThreadId) Nothing
 where
  handleTermination :: Server s => Text -> MVar (ServerState s) -> ThreadId -> IO ()
  handleTermination signalName s serverMainThreadId = do
    modifyMVar_ s $ execStateT (shutdown $ "received " <> signalName)
    -- we are in a forked thread, so to end the server :
    throwTo serverMainThreadId GracefulServerEnd
    -- Note that if this process /also/ hosts a client, the server shutdown
    -- will send the client a 'Disconnected' event. Handling the 'Disconnected'
    -- event will end the program by throwing an exception in the main thread.
    -- But if the client is in a bad state (infinite loop in the main thread),
    -- the program will /not/ terminate, because the 'Disconnected' event will never be handled.
    -- To circumvent this (TODO) we can look at the received events in the thread that
    -- puts the events in the queue, and handle 'Disconnected' from that thread,
    -- throwing an exception to the main thread like we did here. 'ServerEvent'
    -- could be 'Disconnected' | 'NormalEvent' to make it easier to code.

-- Adapted from https://hackage.haskell.org/package/websockets-0.12.3.1/docs/src/Network-WebSockets-Server.html#runServer
runServer' :: Socket -- ^ Listening socket
           -> ServerApp  -- ^ Application
           -> IO ()
runServer' listeningSocket =
  runServerWith' listeningSocket defaultConnectionOptions

-- Adapted from https://hackage.haskell.org/package/websockets-0.12.3.1/docs/src/Network-WebSockets-Server.html#runServer
runServerWith' :: Socket -> ConnectionOptions -> ServerApp -> IO ()
runServerWith' listeningSock opts app =
  forever $ do
    (conn, _) <- accept listeningSock
    void $ forkIO $
      flip finally
        (close conn)
        $ runApp conn opts app

-- Adapted from https://hackage.haskell.org/package/websockets-0.12.3.1/docs/src/Network-WebSockets-Server.html#runServer
runApp :: Socket -> ConnectionOptions -> ServerApp -> IO ()
runApp sock opts app = do
  -- send the data as soon as it's available, to reduce latency.
  setSocketOption sock NoDelay 1
  bracket
    (makePendingConnection sock opts)
    (Stream.close . pendingStream)
    app
