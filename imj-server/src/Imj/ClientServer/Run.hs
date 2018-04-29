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

module Imj.ClientServer.Run
      ( ClientServer(..)
      , ServerState(..)
      , ServerEvent(..)
      , Clients(..)
      , Client(..)
      , ClientInfo(..)
      , ClientId(..)
      , ClientEvent(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      , appSrv
      , handlerError
      , error'
      ) where

import           Imj.Prelude

import           Control.Concurrent(threadDelay, forkIO)
import           Control.Concurrent.MVar.Strict (MVar)
import           Control.Monad.IO.Unlift(MonadUnliftIO)
import           Control.Monad.Reader(runReaderT, asks)
import           Control.Monad.State.Strict(runStateT, execStateT, modify')
import qualified Data.List as List(intercalate)
import qualified Data.Map.Strict as Map
import           Data.Text(pack)
import           Data.Tuple(swap)
import           Network.WebSockets(PendingConnection, Connection, acceptRequest, receiveData, sendBinaryData, sendPing)
import           UnliftIO.Exception (SomeException(..), try)
import           UnliftIO.MVar (modifyMVar_, modifyMVar)

import           Imj.ClientServer.Internal.Types
import           Imj.ClientServer.Class
import           Imj.ClientServer.Types
import           Imj.Graphics.Color.Types
import           Imj.Server.Internal.Types

import           Imj.Graphics.Text.ColorString(colored)
import           Imj.Server.Connection
import           Imj.Server.Log
import           Imj.Timing

appSrv :: (ClientServer s) => MVar (ServerState s) -> PendingConnection -> IO ()
appSrv st pending =
  acceptRequest pending >>= application st

-- | Handles the connection to one client. Synchronous exceptions
-- (network failures or players disconnections) are handled so that
-- a broken connection of /another/ client while broadcasting a message
-- won't impact the handled client.
application :: (ClientServer s) => MVar (ServerState s) -> Connection -> IO ()
application st conn =
  receiveData conn >>= \case
    Connect i cliType -> either
      (\txt ->
          let response = ConnectionRefused i txt
          in sendBinaryData conn response )
      (\_ -> modifyMVar st (fmap swap . runStateT (associateClientId i)) >>= \(cid,lifecycle) ->
               runReaderT (handleClient i cliType lifecycle st) $ ConstClient conn cid)
      $ acceptConnection i
    msg -> error $ "First sent message should be 'Connect'. " ++ show msg


handleClient :: (ClientServer s)
             => ConnectIdT s
             -> ServerOwnership
             -> ConnectionContext s
             -> MVar (ServerState s)
             -> ReaderT ConstClient IO ()
handleClient sn cliType lifecycle st = do
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
      addClient sn cliType
      afterClientWasAdded lifecycle
    forever $ liftIO (receiveData conn) >>=
      modifyMVar_ st . execStateT . logArg handleIncomingEvent'

handleIncomingEvent' :: (ClientServer s)
                     => ClientEvent s
                     -> ClientHandlerIO s ()
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

addClient :: (ClientServer s)
          => ConnectIdT s
          -> ServerOwnership
          -> ClientHandlerIO s ()
addClient sn cliType = do
  conn <- asks connection
  i <- asks clientId
  c' <- createClient i sn
  notifyEveryoneN $ eventsOnWillAddClient i c'

  let c = mkClient conn cliType c'

  modify' $ \ s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
              Map.insert i c $ getClients' clients } }
  serverLog $ (\strId -> colored "Add client" green <> "|" <> strId <> "|" <> showClient c) <$> showId i

  greeters <- map ServerAppEvt <$> greetings
  notifyClientN' $ ConnectionAccepted i : greeters

handlerError :: (ClientServer s)
             => String -> ClientHandlerIO s ()
handlerError = error' "Handler"

error' :: (ClientServer s)
       => String -> String -> ClientHandlerIO s ()
error' from msg = do
  log $ colored (pack txt) red
  notifyClient' $ ServerError txt
  error txt
 where
  txt = List.intercalate "|" [from, "error from Server", msg]
