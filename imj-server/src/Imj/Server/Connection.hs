{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Imj.Server.Connection
      ( notifyN
      , notifyN'
      , notifyEveryone
      , notifyEveryone'
      , notifyEveryoneN
      , notifyEveryoneN'
      , notifyClient
      , notifyClient'
      , notifyClientN
      , notifyClientN'
      , disconnect
      , onBrokenClient
      ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader(asks)
import           Control.Monad.State.Strict(MonadState, modify', gets, state)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Text(pack, justifyRight)
import           Network.WebSockets
                  (WebSocketsData(..), Connection, DataMessage(..),
                  -- the functions on the mext line throw(IO), hence we catch exceptions
                  -- to remove the client from the map when needed.
                   sendClose)
import           Network.WebSockets.Connection(sendDataMessages)
import           UnliftIO.Exception (SomeException(..), try)

import           Imj.ClientView.Internal.Types
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Text.ColorString(intercalate, colored)
import           Imj.Server.Class
import           Imj.Server.Types

import           Imj.Log
import           Imj.Server.Log


{-# INLINABLE sendAndHandleExceptions #-}
sendAndHandleExceptions :: (MonadIO m
                          , Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
                          , MonadState (ServerState s) m)
                        => [ServerEvent s]
                        -> Connection
                        -> ClientId
                        -> m ()
sendAndHandleExceptions [] _ _ = return () -- sendDataMessages throws on empty list
sendAndHandleExceptions evts conn i =
  liftIO (try $ sendDataMessages conn msgs) >>= either
    (\(e :: SomeException) -> onBrokenClient "" (Just ("sending", "ServerEvent", evts)) e i)
    return
 where
  !msgs = map (Binary . toLazyByteString) evts


-- It's important that this function doesn't throw any exception.
onBrokenClient :: (Show a, MonadIO m
                 , Server s, ServerInit s, ServerClientLifecycle s, ServerClientHandler s
                 , MonadState (ServerState s) m)
               => Text
               -- ^ Describes the type of thread in which the exception was raised
               -> Maybe (Text, Text, [a])
               -- ^ A list of values that will be added to the log.
               -> SomeException
               -> ClientId
               -> m ()
onBrokenClient threadCategory infos e i = do
  log'
  disconnect (ClientShutdown $ Left $ pack $ show e) i
 where
  log' = serverLog $
    showId i >>= \strId -> pure $ firstLine strId <> logDetailedException infos e
  firstLine s = intercalate "|"
    [ colored "Exception" (rgb 4 2 0)
    , colored (justifyRight 10 '.' threadCategory) yellow
    , s
    ]

disconnect :: (MonadIO m
             , Server s, ServerInit s, ServerClientLifecycle s, ServerClientHandler s
             , MonadState (ServerState s) m)
           => DisconnectReason
           -> ClientId
           -> m ()
disconnect r i =
  tryRemoveClient >>= maybe
    (do serverLog $ (<> " was already removed.") <$> showId i
        return ())
    (\c@(ClientView _ ownership name _ _) -> do
        -- If the client owns the server, we shutdown connections to /other/ clients first.
        when (ownership == ClientOwnsServer) $ do
          gets clientsMap >>= void . Map.traverseWithKey
            (\j client' -> do
                let msg = "[" <>
                      unClientName name <>
                      "] disconnection (hosts the Server) < " <> pack (show r)
                -- Because we pass a 'ServerShutdown', 'closeConnection' won't
                -- use the clients map so it's safe to just clear the client map
                -- /after/ the entire traversal was done, we won't have infinite recursion.
                closeConnection (ServerShutdown msg) j client')
          removeAllClients
        -- Finally, shutdown the client connection.
        closeConnection r i c)
 where
  closeConnection :: (MonadIO m
                    , Server s, ServerClientLifecycle s, ServerInit s, ServerClientHandler s
                    , MonadState (ServerState s) m)
                  => DisconnectReason -> ClientId -> ClientView (ClientViewT s) -> m ()
  closeConnection reason cid c@(ClientView conn _ name color _) = do
    serverLog $ pure $
      colored "Close connection" yellow <>
      "|" <>
      colored (pack $ show cid) color <>
      "|" <>
      showClient c
    -- If possible, notify the client about the disconnection
    case reason of
      ClientShutdown (Left _) ->
        -- we can't use the client connection anymore.
        -- on its side, the client will probably receive an exception when reading or sending data.
        return ()
      _ -> do
        let e = Disconnected reason
        notify conn cid e
        liftIO $ sendClose conn $
          "[" <>
          unClientName name <>
          "] disconnection < " <> pack (show reason)

    case reason of
      ClientShutdown re -> do
        notifyEveryone' $ RunCommand cid $ Leaves re
        afterClientLeft cid
      ServerShutdown _ ->
        return () -- no need to notify other clients, as they will be diconnected too,
                  -- hence they will receive a server shutdown notification.


  tryRemoveClient = state $ \s ->
    let clients = clientsViews s
        (mayClient, newClients) =
          Map.updateLookupWithKey
            (\_ _ -> Nothing)
            i
            $ views clients
    in (mayClient, s { clientsViews = clients { views = newClients } })

  removeAllClients = modify' $ \s ->
    s { clientsViews = (clientsViews s) { views = Map.empty } }

{-# INLINABLE notify #-}
notify :: (MonadIO m
         , Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
         , MonadState (ServerState s) m)
       => Connection
       -> ClientId
       -> ServerEvent s
       -> m ()
notify conn sid evt =
  sendAndHandleExceptions [evt] conn sid

{-# INLINABLE notifyClientN #-}
notifyClientN :: (Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
                 , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
             => [ServerEventT s]
             -> m ()
notifyClientN = notifyClientN' . map ServerAppEvt

{-# INLINABLE notifyClientN' #-}
notifyClientN' :: (Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
                 , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
             => [ServerEvent s]
             -> m ()
notifyClientN' evts = do
  conn <- asks connection
  sid <- asks clientId
  sendAndHandleExceptions evts conn sid

{-# INLINABLE notifyClient #-}
notifyClient :: (Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
                 , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
             => ServerEventT s
             -> m ()
notifyClient = notifyClient' . ServerAppEvt

{-# INLINABLE notifyClient' #-}
notifyClient' :: (Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
                 , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
             => ServerEvent s
             -> m ()
notifyClient' evt = do
  conn <- asks connection
  sid <- asks clientId
  notify conn sid evt

{-# INLINABLE notifyEveryone #-}
notifyEveryone :: (MonadIO m
                 , Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
                 , MonadState (ServerState s) m)
               => ServerEventT s
               -> m ()
notifyEveryone evt =
  notifyN [evt] =<< gets clientsMap

{-# INLINABLE notifyEveryone' #-}
notifyEveryone' :: (MonadIO m
                  , Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
                  , MonadState (ServerState s) m)
                => ServerEvent s
                -> m ()
notifyEveryone' evt =
  notifyN' [evt] =<< gets clientsMap

{-# INLINABLE notifyEveryoneN #-}
notifyEveryoneN :: (MonadIO m
                  , Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
                  , MonadState (ServerState s) m)
                => [ServerEventT s]
                -> m ()
notifyEveryoneN evts =
  notifyN evts =<< gets clientsMap

{-# INLINABLE notifyEveryoneN' #-}
notifyEveryoneN' :: (MonadIO m
                   , Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
                   , MonadState (ServerState s) m)
                => [ServerEvent s]
                -> m ()
notifyEveryoneN' evts =
  notifyN' evts =<< gets clientsMap

-- | Uses sendDataMessage which is at a lower-level than sendBinaryData
-- to factorize serialization.
--
-- In case some connections are closed, the corresponding clients are silently removed
-- from the Map, and we continue the processing.
{-# INLINABLE notifyN #-}
notifyN :: (MonadIO m
          , Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
          , MonadState (ServerState s) m)
        => [ServerEventT s]
        -> Map ClientId (ClientView c)
        -> m ()
notifyN evts = notifyN' (map ServerAppEvt evts)

{-# INLINABLE notifyN' #-}
notifyN' :: (MonadIO m
          , Server s, ServerClientHandler s, ServerInit s, ServerClientLifecycle s
          , MonadState (ServerState s) m)
        => [ServerEvent s]
        -> Map ClientId (ClientView c)
        -> m ()
notifyN' evts m =
  void $ Map.traverseWithKey
    (\i client -> sendAndHandleExceptions evts (getConnection client) i) m
