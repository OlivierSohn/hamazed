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

import           Control.Monad.IO.Unlift(MonadUnliftIO, MonadIO)
import           Control.Monad.Reader(runReaderT, lift, asks)
import           Control.Monad.State.Strict(StateT, MonadState, runStateT, execStateT, modify', get, gets, state)
import qualified Data.Map.Strict as Map
import           Data.Text(pack)

import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Color
import           Imj.Log
import           Imj.Server.Types
import           Imj.Server.Internal.Types


findClient :: ClientId -> ServerState s -> Maybe (Client (ClientT s))
findClient i s = Map.lookup i $ clientsMap s

showId :: (ClientInfo (ClientT s), MonadState (ServerState s) m)
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

log :: (ClientInfo (ClientT s))
    => ColorString -> (ClientHandlerIO (ServerState s)) ()
log msg = gets serverLogs >>= \case
  NoLogs -> return ()
  ConsoleLogs -> do
    i <- asks clientId
    idStr <- showId i
    liftIO $ baseLog $ intercalate "|"
      [ idStr
      , msg
      ]

warning :: (ClientInfo (ClientT s))
        => Text -> (ClientHandlerIO (ServerState s)) ()
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
withArgLogged :: (Show a, ClientInfo (ClientT s))
              => (a -> ClientHandlerIO (ServerState s) b)
              -> a
              -> ClientHandlerIO (ServerState s) b
withArgLogged act arg = do
  log $ colored " >> " (gray 18) <> keepExtremities (show arg)
  res <- act arg
  log $ colored " <<" (gray 18)
  return res
