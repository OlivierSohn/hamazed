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
      , logArg
      , showId
      , showClient
      ) where

import           Imj.Prelude

import           Control.Monad.Reader(asks)
import           Control.Monad.State.Strict(gets)
import qualified Data.Map.Strict as Map
import           Data.Text(pack)

import           Imj.Server.Class
import           Imj.ClientView.Internal.Types
import           Imj.Server.Types

import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Color
import           Imj.Log

findClient :: ClientId -> ServerState s -> Maybe (ClientView (ClientViewT s))
findClient i s = Map.lookup i $ clientsMap s

showId :: (MonadState (ServerState s) m)
       => ClientId
       -> m ColorString
showId i =
  colored (pack $ show i) . fromMaybe (gray 16) . fmap getColor <$> gets (findClient i)

serverLog :: (MonadIO m, MonadState (ServerState s) m)
          => m ColorString
          -> m ()
serverLog msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs ->
    msg >>= baseLog

{-# INLINABLE showClient #-}
showClient :: (Show c) => c -> ColorString
showClient c = colored (pack $ show c) $ gray 16

log :: (MonadIO m
      , MonadState (ServerState s) m, MonadReader ConstClientView m)
    => ColorString -> m ()
log msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs -> do
    i <- asks clientId
    idStr <- showId i
    liftIO $ baseLog $ intercalate "|"
      [ idStr
      , msg
      ]

warning :: (MonadIO m
          , MonadState (ServerState s) m, MonadReader ConstClientView m)
        => Text -> m ()
warning msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs -> do
    i <- asks clientId
    idStr <- showId i
    liftIO $ baseLog $ intercalate "|"
      [ idStr
      , colored msg orange
      ]

{-# INLINABLE logArg #-}
logArg :: (Show a
          , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
       => (a -> m b)
       -> a
       -> m b
logArg act arg = do
  log $ colored " >> " (gray 18) <> keepExtremities (show arg)
  res <- act arg
  log $ colored " <<" (gray 18)
  return res
