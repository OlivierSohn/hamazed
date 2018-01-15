{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Input.FromMonadReader
       ( -- * Player input
         getPlayerKey
       , getPlayerKeyTimeout
       , tryGetPlayerKey
       -- * Reexports
       , MonadReader, MonadIO
       ) where

import           Imj.Prelude

import           Control.Monad(join)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader, asks)

import           Imj.Input.Types


{-# INLINABLE getPlayerKey #-}
getPlayerKey :: (PlayerInput i, MonadReader i m, MonadIO m)
             => m Key
getPlayerKey = do
  join(asks getKey)

{-# INLINABLE getPlayerKeyTimeout #-}
getPlayerKeyTimeout :: (PlayerInput i, MonadReader i m, MonadIO m)
                    => SystemTime
                    -> Int
                    -> m (Maybe Key)
getPlayerKeyTimeout curTime ms = do
  d <- asks getKeyTimeout
  d curTime ms

{-# INLINABLE tryGetPlayerKey #-}
tryGetPlayerKey :: (PlayerInput i, MonadReader i m, MonadIO m)
                => m (Maybe Key)
tryGetPlayerKey =
  join(asks tryGetKey)
