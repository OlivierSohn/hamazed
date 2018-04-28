{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Server.Log
      ( log
      , warning
      , serverLog
      , withArgLogged
      , showId
      , showClient
      , logColor
      ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader(asks)
import           Control.Monad.State.Strict(MonadState, gets)
import qualified Data.Map.Strict as Map
import           Data.Text(pack)

import           Imj.ClientServer.Class
import           Imj.Server.Types
import           Imj.Server.Internal.Types

import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Color
import           Imj.Log


findClient :: ClientId -> ServerState s -> Maybe (Client (ClientT s))
findClient i s = Map.lookup i $ clientsMap s

showId :: (ClientServer s, MonadState (ServerState s) m)
       => ClientId
       -> m ColorString
showId i =
  colored (pack $ show i) . fromMaybe (gray 16) . join . fmap clientLogColor . fmap unClient <$> gets (findClient i)

serverLog :: (MonadIO m, MonadState (ServerState s) m)
          => m ColorString
          -> m ()
serverLog msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs ->
    msg >>= baseLog

logColor :: (ClientInfo c) => c -> Color8 Foreground
logColor c = fromMaybe (gray 16) $ clientLogColor c

{-# INLINABLE showClient #-}
showClient :: (Show c) => c -> ColorString
showClient c = colored (pack $ show c) $ gray 16

log :: ClientServer s
    => ColorString -> (ClientHandlerIO s) ()
log msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs -> do
    i <- asks clientId
    idStr <- showId i
    liftIO $ baseLog $ intercalate "|"
      [ idStr
      , msg
      ]

warning :: ClientServer s
        => Text -> (ClientHandlerIO s) ()
warning msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs -> do
    i <- asks clientId
    idStr <- showId i
    liftIO $ baseLog $ intercalate "|"
      [ idStr
      , colored msg orange
      ]

{-# INLINABLE withArgLogged #-}
withArgLogged :: (Show a, ClientServer s)
              => (a -> ClientHandlerIO s b)
              -> a
              -> ClientHandlerIO s b
withArgLogged act arg = do
  log $ colored " >> " (gray 18) <> keepExtremities (show arg)
  res <- act arg
  log $ colored " <<" (gray 18)
  return res
