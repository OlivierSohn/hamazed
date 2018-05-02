{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Network
      ( sendToServer
      ) where

import           Imj.Prelude
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader, asks)

import           Imj.Game.Hamazed.State.Types
import           Imj.Server.Types

{-# INLINABLE sendToServer #-}
sendToServer :: (MonadReader e m, Client e
               , MonadIO m)
             => ClientEventT (ServerT (GameLogicT e))
             -> m ()
sendToServer e =
  asks sendToServer' >>= \f -> f (ClientAppEvt e)
