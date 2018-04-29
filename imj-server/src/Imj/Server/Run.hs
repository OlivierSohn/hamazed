{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Server.Run
      ( Server(..)
      , ServerState(..)
      , ServerEvent(..)
      , ClientInfo(..)
      , ClientId(..)
      , ClientEvent(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      , appSrv
      , shutdown
      , handlerError
      , error'
      ) where

import           Imj.Prelude

import           Control.Concurrent(threadDelay, forkIO)
import           Control.Concurrent.MVar.Strict (MVar)
import           Control.Monad.IO.Unlift(MonadIO, MonadUnliftIO)
import           Control.Monad.Reader(runReaderT, asks)
import           Control.Monad.State.Strict(StateT, runStateT, execStateT, modify', state, gets)
import qualified Data.List as List(intercalate)
import qualified Data.Map.Strict as Map
import           Data.Text(pack)
import           Data.Tuple(swap)
import           Network.WebSockets(PendingConnection, Connection, acceptRequest, receiveData, sendBinaryData, sendPing)
import           UnliftIO.Exception (SomeException(..), try)
import           UnliftIO.MVar (modifyMVar_, modifyMVar)

import           Imj.Graphics.Color.Types
import           Imj.Server.Internal.Types
import           Imj.Server.Class
import           Imj.Server.Types

import           Imj.Graphics.Text.ColorString(colored)
import           Imj.Server.Connection
import           Imj.Server.Log
import           Imj.Timing

appSrv :: (Server s) => MVar (ServerState s) -> PendingConnection -> IO ()
appSrv st pending =
  acceptRequest pending >>= application st

-- | Handles the connection to one client. Synchronous exceptions
-- (network failures or players disconnections) are handled so that
-- a broken connection of /another/ client while broadcasting a message
-- won't impact the handled client.
application :: Server s => MVar (ServerState s) -> Connection -> IO ()
application st conn =
  receiveData conn >>= \case
    msg@ClientAppEvt {} -> error $ "First sent message should be 'Connect'. " ++ show msg
    Connect connectId cliType ->
      either
        refuse
        (\_ ->
          modifyMVar st (fmap swap . runStateT associateClientId) >>=
            either
              refuse
                (\(cid,lifecycle) ->
                  runReaderT (handleClient connectId cliType lifecycle st) $ ConstClient conn cid))
      $ acceptConnection connectId

     where

      refuse txt =
        let response = ConnectionRefused connectId txt
        in sendBinaryData conn response

      associateClientId = do
        term <- gets shouldTerminate
        if term
          then
            return $ Left "Server is shutting down"
          else
            tryReconnect connectId >>= fmap Right . (maybe
              (state $ \s ->
                let (clients,newId) = takeClientId $ clientsViews s
                in ( (newId, NewClient)
                   , s { clientsViews = clients } ))
              (return . fmap ReconnectingClient))

       where

        takeClientId :: ClientViews c -> (ClientViews c, ClientId)
        takeClientId (ClientViews c i) = (ClientViews c $ succ i, i)


handleClient :: (Server s)
             => ConnectIdT s
             -> ServerOwnership
             -> ClientLifecycle (ReconnectionContext s)
             -> MVar (ServerState s)
             -> ReaderT ConstClient IO ()
handleClient connectId cliType lifecycle st = do
  i <- asks clientId
  let disconnectOnException :: (MonadUnliftIO m) => Text -> m () -> m ()
      disconnectOnException name action = try action >>= either
        (\(e :: SomeException) ->
            liftIO $ modifyMVar_ st $ execStateT $
              onBrokenClient name (Nothing :: Maybe (Text, Text, [Text])) e i)
        return
  conn <- asks connection
  -- To detect disconnections when communication is idle:
  void $ liftIO $ forkIO $ disconnectOnException "PingPong" $ pingPong conn $ fromSecs 1
  disconnectOnException "Handler" $ do
    modifyMVar_ st $ execStateT $ do
      addClient connectId cliType
      case lifecycle of
        NewClient ->
          log "Is a new client"
        ReconnectingClient c -> do
          log "Is a reconnecting client"
          onReconnection c

    forever $ liftIO (receiveData conn) >>=
      modifyMVar_ st . execStateT . logArg handleIncomingEvent'

handleIncomingEvent' :: (Server s
                       , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClient m)
                     => ClientEvent s
                     -> m ()
handleIncomingEvent' = \case
  Connect i _ ->
    handlerError $ "already connected : " ++ show i
  ClientAppEvt e -> handleClientEvent e

pingPong :: Connection -> Time Duration System -> IO ()
pingPong conn dt =
  go 0
 where
  go :: Int -> IO ()
  go i = do
    threadDelay $ fromIntegral $ toMicros dt
    sendPing conn $ pack $ show i
    go $ i + 1 -- it can overflow, that is ok.

addClient :: (Server s
            , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClient m)
          => ConnectIdT s
          -> ServerOwnership
          -> m ()
addClient connectId cliType = do
  conn <- asks connection
  i <- asks clientId
  c' <- createClientView i connectId
  notifyEveryoneN $ announceNewcomer i c'

  let c = ClientView conn cliType c'

  modify' $ \ s ->
    let clients = clientsViews s
    in s { clientsViews =
            clients { views =
              Map.insert i c $ views clients } }
  serverLog $ (\strId -> colored "Add client" green <> "|" <> strId <> "|" <> showClient c) <$> showId i

  greeters <- map ServerAppEvt <$> greetNewcomer
  notifyClientN' $ ConnectionAccepted i : greeters

handlerError :: (Server s
                , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClient m)
             => String -> m ()
handlerError = error' "Handler"

error' :: (Server s
         , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClient m)
       => String -> String -> m ()
error' from msg = do
  log $ colored (pack txt) red
  notifyClient' $ ServerError txt
  error txt
 where
  txt = List.intercalate "|" [from, "error from Server", msg]

shutdown :: Server s
         => Text
         -> StateT (ServerState s) IO ()
shutdown reason = do
  serverLog $ pure $ colored "Server shutdown" red <> "|" <> colored reason (rgb 4 1 1)
  modify' $ \s -> s { shouldTerminate = True }
  Map.keysSet <$> gets clientsMap >>= mapM_ (disconnect $ ServerShutdown reason)
