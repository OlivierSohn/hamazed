{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Network.Notify
      ( notifyPlayersN
      , notifyPlayers
      ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.State.Strict(MonadState, gets)

import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types

import           Imj.Server.Connection

{-# INLINABLE notifyPlayersN #-}
notifyPlayersN :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
               => [ServerEventT HamazedServerState] -> m ()
notifyPlayersN evts =
  notifyN evts =<< gets onlyPlayersMap

{-# INLINABLE notifyPlayers #-}
notifyPlayers :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
              => ServerEventT HamazedServerState -> m ()
notifyPlayers evt =
  notifyN [evt] =<< gets onlyPlayersMap
