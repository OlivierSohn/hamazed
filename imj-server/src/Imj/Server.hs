{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Server
      ( adjustClient
      , adjustAllWithKey
      , adjustAll
      , adjustAll'
      , ServerState
      , serverError
      , clientsMap
      , mapState
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

{-# INLINE mapState #-}
mapState :: (s -> s) -> ServerState s -> ServerState s
mapState f s = s { unServerState = f $ unServerState s }

{-# INLINABLE adjustAllWithKey #-}
adjustAllWithKey :: (MonadState (ServerState s) m)
                 => (ClientId -> ClientViewT s -> ClientViewT s) -> m ()
adjustAllWithKey f =
  modify' $ \s ->
    let clients = clientsViews s
    in s { clientsViews =
            clients { views =
                        Map.mapWithKey
                          (\k -> fmap (f k)) $
                          views clients
                    }
          }

{-# INLINABLE adjustAll #-}
adjustAll :: (MonadState (ServerState s) m)
          => (ClientViewT s -> ClientViewT s) -> m ()
adjustAll f =
  modify' $ \s ->
    let clients = clientsViews s
    in s { clientsViews =
            clients { views =
                        Map.map (fmap f) $ views clients
                    }
          }

{-# INLINABLE adjustAll' #-}
adjustAll' :: (MonadState (ServerState s) m)
           => (ClientViewT s -> Maybe (ClientViewT s)) -> m (Set ClientId)
adjustAll' f =
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
adjustClient f = do
  i <- asks clientId
  modify' $ \s ->
    let clients = clientsViews s
    in s { clientsViews =
            clients { views =
                        Map.adjust (fmap f) i $ views clients
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
