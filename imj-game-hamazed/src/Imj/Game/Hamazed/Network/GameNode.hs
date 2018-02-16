{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Hamazed.Network.GameNode
      ( startClient
      , startServerIfLocal
      ) where

import           Imj.Prelude
import           Control.Concurrent.STM(newTQueueIO)
import           Control.Concurrent (MVar, ThreadId, forkIO, newMVar, putMVar, modifyMVar_, myThreadId, throwTo)
import           Control.Exception (onException, finally, bracket)
import           Control.Monad.State.Strict(execStateT)
import           Foreign.C.Types(CInt)
import           Network.Socket(Socket, close, accept)
import           Network.WebSockets(ServerApp, ConnectionOptions, defaultConnectionOptions,
                    makeListenSocket, makePendingConnection, runClient)
import           Network.WebSockets.Connection(PendingConnection(..))
import qualified Network.WebSockets.Stream as Stream(close)
import           System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM)

import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.Network.Class.ClientNode
import           Imj.Game.Hamazed.Network.Client(appCli)
import           Imj.Game.Hamazed.Network.Server(appSrv, gameScheduler, shutdown)

startServerIfLocal :: Server
                   -> MVar ()
                   -- ^ Will be set when the client can connect to the server.
                   -> IO ()
startServerIfLocal (Distant _ _) v = putMVar v () >> return () -- TODO we could ping the ip?
startServerIfLocal srv@(Local _) v = do
  let (ServerName host, ServerPort port) = getServerNameAndPort srv
  listen <- makeListenSocket host port `onException` (putStrLn $ failure msg)
  putMVar v () -- now that the listen socket is created, signal it.
  putStrLn $ success msg
  newServerState >>= newMVar >>= \state -> do
    serverMainThread <- myThreadId
    mapM_ (installOneHandler state serverMainThread)
          [(sigINT,  "sigINT")
         , (sigTERM, "sigTERM")]
    -- fork 1 thread to schedule the game
    _ <- forkIO $ gameScheduler state
    -- the current thread listens for incomming connections, 1 thread is forked per client
    runServer' listen $ appSrv state
 where
  msg x = "Hamazed GameServer " ++ st x ++ show srv ++ ")"
   where
    st False = "failed to start ("
    st True = "started ("

startClient :: SuggestedPlayerName -> Server -> IO ClientQueues
startClient playerName srv = do
  -- by now, if the server is local, the listening socket has been created.
  qs <- mkQueues
  -- start client
  let (ServerName name, ServerPort port) = getServerNameAndPort srv
  _ <- forkIO $
    runClient name port "/" $ \x -> do
      putStrLn $ success msg-- there is no corresponding 'failure', we will see the exception in the console.
      appCli qs x
  -- initialize the game connection
  sendToServer' qs $
    Connect playerName $
      case srv of
        Local _     -> ClientType ClientOwnsServer
        Distant _ _ -> ClientType FreeServer
  return qs
 where
  msg x = "Hamazed GameClient " ++ st x ++ " to Hamazed GameServer (" ++ show srv ++ ")"
   where
    st False = "failed to connect"
    st True = "connected"

success, failure :: (Bool -> String) -> String
success txt = "Info|"  ++ txt True
failure txt = "ERROR|" ++ txt False

mkQueues :: IO ClientQueues
mkQueues =
  ClientQueues <$> newTQueueIO <*> newTQueueIO

installOneHandler :: MVar ServerState -> ThreadId -> (CInt, Text) -> IO ()
installOneHandler state serverThreadId (sig,sigName) =
  void $ installHandler sig (Catch $ handleTermination sigName state serverThreadId) Nothing
 where
  handleTermination :: Text -> MVar ServerState -> ThreadId -> IO ()
  handleTermination signalName s serverMainThreadId = do
    modifyMVar_ s $ execStateT (shutdown $ "received " <> signalName)
    -- we are in a forked thread, so to end the server :
    throwTo serverMainThreadId GracefulServerEnd
    -- Note that if this process also hosts a client, the client will be disconnected during the
    -- server shutdown, and this will end the program by throwing an exception in the main thread.

-- Adapted from https://hackage.haskell.org/package/websockets-0.12.3.1/docs/src/Network-WebSockets-Server.html#runServer
-- (I needed to know when the listening socket was ready)
runServer' :: Socket -- ^ Listening socket
           -> ServerApp  -- ^ Application
           -> IO ()
runServer' listeningSocket app =
  runServerWith' listeningSocket defaultConnectionOptions app

-- Adapted from https://hackage.haskell.org/package/websockets-0.12.3.1/docs/src/Network-WebSockets-Server.html#runServer
runServerWith' :: Socket -> ConnectionOptions -> ServerApp -> IO ()
runServerWith' listeningSock opts app =
  forever $ do
    (conn, _) <- accept listeningSock
    void $ forkIO $
      flip finally
        (close conn)
        $ runApp conn opts app

-- Copied from https://hackage.haskell.org/package/websockets-0.12.3.1/docs/src/Network-WebSockets-Server.html#runServer
runApp :: Socket -> ConnectionOptions -> ServerApp -> IO ()
runApp socket opts =
  bracket
    (makePendingConnection socket opts)
    (Stream.close . pendingStream)
