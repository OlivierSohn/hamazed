{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Server
      ( adjustClient
      , adjustClient'
      , adjustAllClientsWithKey
      , adjustAllClientsWithKey'
      , adjustAllClients
      , adjustAllClients'
      , ServerState
      , serverError
      , clientsMap
      ) where

import           Imj.Prelude
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader(asks)
import           Control.Monad.State.Strict(MonadState, modify', state)
import qualified Data.List as List(intercalate)
import qualified Data.Map.Strict as Map
import           Data.Set(Set)
import           Data.Text(pack)

import           Imj.ClientView.Types
import           Imj.Server.Class
import           Imj.Server.Types

import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Color
import           Imj.Server.Connection
import           Imj.Server.Log

{-# INLINABLE adjustAllClientsWithKey #-}
adjustAllClientsWithKey :: (MonadState (ServerState s) m)
                 => (ClientId -> ClientViewT s -> ClientViewT s) -> m ()
adjustAllClientsWithKey f =
  modify' $ \s ->
    let clients = clientsViews s
    in s { clientsViews =
            clients { views =
                        Map.mapWithKey (\k -> fmap (f k)) $ views clients
                    }
          }

{-# INLINABLE adjustAllClientsWithKey' #-}
adjustAllClientsWithKey' :: (MonadState (ServerState s) m)
                 => (ClientId -> ClientView (ClientViewT s) -> ClientView (ClientViewT s)) -> m ()
adjustAllClientsWithKey' f =
  modify' $ \s ->
    let clients = clientsViews s
    in s { clientsViews =
            clients { views =
                        Map.mapWithKey (\k -> f k) $ views clients
                    }
          }

{-# INLINABLE adjustAllClients #-}
adjustAllClients :: (MonadState (ServerState s) m)
          => (ClientViewT s -> ClientViewT s) -> m ()
adjustAllClients f =
  modify' $ \s ->
    let clients = clientsViews s
    in s { clientsViews =
            clients { views =
                        Map.map (fmap f) $ views clients
                    }
          }

{-# INLINABLE adjustAllClients' #-}
adjustAllClients' :: (MonadState (ServerState s) m)
           => (ClientViewT s -> Maybe (ClientViewT s)) -> m (Set ClientId)
adjustAllClients' f =
  state $ \s ->
    let clients = clientsViews s
        m = views clients
        changed = Map.mapMaybe
          (\c -> maybe Nothing (\r -> Just $ c {unClientView = r}) $ f $ unClientView c)
          m
        newM = Map.union changed m -- left biased, so new elements will delete old ones.
    in (Map.keysSet changed
      , s { clientsViews = clients { views = newM } })

{-# INLINABLE adjustClient #-}
adjustClient :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
             => (ClientViewT s -> ClientViewT s) -> m ()
adjustClient = adjustClient' . fmap

{-# INLINABLE adjustClient' #-}
adjustClient' :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
             => (ClientView (ClientViewT s) -> ClientView (ClientViewT s)) -> m ()
adjustClient' f = do
  i <- asks clientId
  modify' $ \s ->
    let clients = clientsViews s
    in s { clientsViews =
            clients { views =
                        Map.adjust f i $ views clients
                    }
          }

serverError ::(MonadIO m
          , Server s
          , MonadState (ServerState s) m)
          => String
            -> m ()
serverError msg = do
  serverLog $ pure $ colored (pack txt) red
  notifyEveryone' $ ServerError txt
  error txt
 where
  txt = List.intercalate "|" ["Server error from Server", msg]
