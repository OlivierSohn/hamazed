{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Input.FromMonadReader
       ( -- * Player input
         getPlayerKey
       , getPlayerKeyTimeout
       , tryGetPlayerKey
       , hasPlayerKey
       , unGetPlayerKey
       , playerEndsProgram
       -- * Reexports
       , MonadReader, MonadIO, Int64
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

{-# INLINABLE unGetPlayerKey #-}
unGetPlayerKey :: (PlayerInput i, MonadReader i m, MonadIO m)
               => Key
               -> m ()
unGetPlayerKey k = do
  d <- asks unGetKey
  d k

{-# INLINABLE getPlayerKeyTimeout #-}
getPlayerKeyTimeout :: (PlayerInput i, MonadReader i m, MonadIO m)
                    => Time Point System
                    -> Time Duration System
                    -> m (Maybe Key)
getPlayerKeyTimeout curTime ms = do
  d <- asks getKeyTimeout
  d curTime ms

{-# INLINABLE tryGetPlayerKey #-}
tryGetPlayerKey :: (PlayerInput i, MonadReader i m, MonadIO m)
                => m (Maybe Key)
tryGetPlayerKey =
  join(asks tryGetKey)

{-# INLINABLE hasPlayerKey #-}
hasPlayerKey :: (PlayerInput i, MonadReader i m, MonadIO m)
             => m Bool
hasPlayerKey =
  join(asks someInputIsAvailable)

{-# INLINABLE playerEndsProgram #-}
playerEndsProgram :: (PlayerInput i, MonadReader i m, MonadIO m)
                  => m Bool
playerEndsProgram =
  join(asks programShouldEnd)
