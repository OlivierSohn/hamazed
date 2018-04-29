{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Imj.Server.Connection
      ( notify
      , notifyN
      , notifyN'
      , notifyEveryone
      , notifyEveryone'
      , notifyEveryoneN
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

import           Imj.Graphics.Color.Types
import           Imj.Graphics.Text.ColorString(intercalate, colored)
import           Imj.ClientServer.Class
import           Imj.ClientServer.Internal.Types
import           Imj.ClientServer.Types
import           Imj.Server.Types

import           Imj.Log
import           Imj.Server.Log


{-# INLINABLE sendAndHandleExceptions #-}
sendAndHandleExceptions :: (MonadIO m
                          , ClientServer s
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
                 , ClientServer s
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

disconnect :: (MonadIO m, ClientServer s, MonadState (ServerState s) m)
           => DisconnectReason
           -> ClientId
           -> m ()
disconnect r i =
  tryRemoveClient >>= maybe
    (do serverLog $ (<> " was already removed.") <$> showId i
        return ())
    (\c@(Client _ ownership innerC) -> do
        -- If the client owns the server, we shutdown connections to /other/ clients first.
        when (ownership == ClientOwnsServer) $ do
          gets clientsMap >>= void . Map.traverseWithKey
            (\j client' -> do
                let msg = "[" <>
                      nameWithFallback i innerC <>
                      "] disconnection (hosts the Server) < " <> pack (show r)
                -- Because we pass a 'ServerShutdown', 'closeConnection' won't
                -- use the clients map so it's safe to just clear the client map
                -- /after/ the entire traversal was done, we won't have infinite recursion.
                closeConnection (ServerShutdown msg) j client')
          removeAllClients
        -- Finally, shutdown the client connection.
        closeConnection r i c)
 where
  closeConnection :: (MonadIO m, ClientServer s, MonadState (ServerState s) m)
                  => DisconnectReason -> ClientId -> Client (ClientT s) -> m ()
  closeConnection reason cid c@(Client conn _ c') = do
    serverLog $ pure $
      colored "Close connection" yellow <>
      "|" <>
      colored (pack $ show cid) (logColor c') <>
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
          nameWithFallback cid c' <>
          "] disconnection < " <> pack (show reason)

    afterClientLeft cid reason

  tryRemoveClient = state $ \s ->
    let clients = getClients s
        (mayClient, newClients) =
          Map.updateLookupWithKey
            (\_ _ -> Nothing)
            i
            $ getClients' clients
    in (mayClient, s { getClients = clients { getClients' = newClients } })

  removeAllClients = modify' $ \s ->
    s { getClients = (getClients s) { getClients' = Map.empty } }

  nameWithFallback cid c = fromMaybe (pack $ show cid) $ clientFriendlyName c

{-# INLINABLE notify #-}
notify :: (MonadIO m
         , ClientServer s, MonadState (ServerState s) m)
       => Connection
       -> ClientId
       -> ServerEvent s
       -> m ()
notify conn sid evt =
  sendAndHandleExceptions [evt] conn sid

{-# INLINABLE notifyClientN #-}
notifyClientN :: (ClientServer s
                 , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClient m)
             => [ServerEventT s]
             -> m ()
notifyClientN = notifyClientN' . map ServerAppEvt

{-# INLINABLE notifyClientN' #-}
notifyClientN' :: (ClientServer s
                 , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClient m)
             => [ServerEvent s]
             -> m ()
notifyClientN' evts = do
  conn <- asks connection
  sid <- asks clientId
  sendAndHandleExceptions evts conn sid

{-# INLINABLE notifyClient #-}
notifyClient :: (ClientServer s
                 , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClient m)
             => ServerEventT s
             -> m ()
notifyClient = notifyClient' . ServerAppEvt

{-# INLINABLE notifyClient' #-}
notifyClient' :: (ClientServer s
                 , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClient m)
             => ServerEvent s
             -> m ()
notifyClient' evt = do
  conn <- asks connection
  sid <- asks clientId
  notify conn sid evt

{-# INLINABLE notifyEveryone #-}
notifyEveryone :: (MonadIO m, ClientServer s, MonadState (ServerState s) m)
               => ServerEventT s
               -> m ()
notifyEveryone evt =
  notifyN [evt] =<< gets clientsMap

{-# INLINABLE notifyEveryone' #-}
notifyEveryone' :: (MonadIO m, ClientServer s, MonadState (ServerState s) m)
                => ServerEvent s
                -> m ()
notifyEveryone' evt =
  notifyN' [evt] =<< gets clientsMap

{-# INLINABLE notifyEveryoneN #-}
notifyEveryoneN :: (MonadIO m, ClientServer s, MonadState (ServerState s) m)
                => [ServerEventT s]
                -> m ()
notifyEveryoneN evts =
  notifyN evts =<< gets clientsMap

-- | Uses sendDataMessage which is at a lower-level than sendBinaryData
-- to factorize serialization.
--
-- In case some connections are closed, the corresponding clients are silently removed
-- from the Map, and we continue the processing.
{-# INLINABLE notifyN #-}
notifyN :: (MonadIO m
          , ClientServer s
          , MonadState (ServerState s) m)
        => [ServerEventT s]
        -> Map ClientId (Client c)
        -> m ()
notifyN evts = notifyN' (map ServerAppEvt evts)

{-# INLINABLE notifyN' #-}
notifyN' :: (MonadIO m
          , ClientServer s
          , MonadState (ServerState s) m)
        => [ServerEvent s]
        -> Map ClientId (Client c)
        -> m ()
notifyN' evts m =
  void $ Map.traverseWithKey
    (\i client -> sendAndHandleExceptions evts (getConnection client) i) m
