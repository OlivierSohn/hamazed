{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Server
      ( ClientNode(..)
      , send
      ) where

import           Control.Concurrent.STM(TQueue)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader, asks)

import           Imj.Game.Hamazed.Loop.Event.Types

-- | A 'ClientNode' sends 'ClientEvent's and receives 'ServerEvent's.
class ClientNode a where
  send' :: (MonadIO m) => a -> ClientEvent -> m ()
  serverQueue :: a -> TQueue ServerEvent


{-# INLINABLE send #-}
send :: (ClientNode i, MonadReader i m, MonadIO m)
     => ClientEvent
     -> m ()
send e = do
  s <- asks send'
  s e
