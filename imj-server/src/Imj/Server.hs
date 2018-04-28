{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Server
      (
      -- * For server
        adjustAllWithKey
      , adjustAll
      , adjustAll'
      , ServerState
      , serverError
      , clientsMap
      , shutdown
      -- * For client
      , getServerNameAndPort
      ) where

import           Imj.Prelude
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader(runReaderT, lift, asks)
import           Control.Monad.State.Strict(StateT, MonadState, runStateT, execStateT, modify', get, gets, state)
import qualified Data.Binary as Bin
import           Data.Int(Int64)
import qualified Data.List as List(intercalate)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)
import           Data.Set(Set)
import           Data.String(IsString)
import           Data.Text.Lazy.Encoding as LazyT
import qualified Data.Text.Lazy as LazyT
import           Data.Text(unpack, pack)
import           Network.WebSockets

import           Imj.Server.Types
import           Imj.Server.Internal.Types

import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Color
import           Imj.Server.Connection
import           Imj.Server.Log

--------------------------------------------------------------------------------
---------- Seen from a server's perspective ------------------------------------
--------------------------------------------------------------------------------


{-# INLINABLE adjustAllWithKey #-}
adjustAllWithKey :: (MonadState (ServerState s) m) => (ClientId -> ClientT s -> ClientT s) -> m ()
adjustAllWithKey f =
  modify' $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.mapWithKey
                          (\k -> fmap (f k)) $
                          getClients' clients
                    }
          }

{-# INLINABLE adjustAll #-}
adjustAll :: (MonadState (ServerState s) m) => (ClientT s -> ClientT s) -> m ()
adjustAll f =
  modify' $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.map (fmap f) $ getClients' clients
                    }
          }

{-# INLINABLE adjustAll' #-}
adjustAll' :: (MonadState (ServerState s) m) => (ClientT s -> Maybe (ClientT s)) -> m (Set ClientId)
adjustAll' f =
  state $ \s ->
    let clients = getClients s
        m = getClients' clients
        changed = Map.mapMaybe
          (\c -> maybe Nothing (\r -> Just $ c {unClient = r}) $ f $ unClient c)
          m
        newM = Map.union changed m -- left biased, so new elements will delete old ones.
    in (Map.keysSet changed
      , s { getClients = clients { getClients' = newM } })

adjustClient :: (ClientT s -> ClientT s) -> ClientHandlerIO (ServerState s) ()
adjustClient f = do
  i <- lift $ asks clientId
  modify' $ \s ->
    let clients = getClients s
    in s { getClients =
            clients { getClients' =
                        Map.adjust (fmap f) i $ getClients' clients
                    }
          }

shutdown :: (Binary (ConnectIdT s), Show (ServerEventT s), Binary (ServerEventT s), ClientInfo (ClientT s), ServerImpl s)
         => Text -> StateT (ServerState s) IO ()
shutdown reason = do
  serverLog $ pure $ colored "Server shutdown" red <> "|" <> colored reason (rgb 4 1 1)
  modify' $ \s -> s { shouldTerminate = True }
  Map.keysSet <$> gets clientsMap >>= mapM_ (disconnect $ ServerShutdown reason)

serverError ::(MonadIO m
          , ServerImpl s
          , Binary (ConnectIdT s), Binary (ServerEventT s), Show (ServerEventT s), ClientInfo (ClientT s)
          , MonadState (ServerState s) m)
          => String
            -> m ()
serverError msg = do
  serverLog $ pure $ colored (pack txt) red
  s <- get
  notifyN' [ServerError txt] =<< gets clientsMap
  --notifyEveryone (ServerError txt)
  error txt
 where
  txt = List.intercalate "|" ["Server error from Server", msg]

--------------------------------------------------------------------------------
---------- Seen from a client's perspective ------------------------------------
--------------------------------------------------------------------------------

getServerNameAndPort :: Server c -> (ServerName, ServerPort)
getServerNameAndPort (Server (Local _) (ServerContent p _)) = (ServerName "localhost", p)
getServerNameAndPort (Server (Distant name) (ServerContent p _)) = (name, p)
