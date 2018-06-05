{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Control.Concurrent.AsyncGroups.Class
    ( AsyncGroups(..)
    ) where

import           Imj.Prelude

import           Control.Concurrent.Async (Async)

class AsyncGroups a where
  -- | Attaches an 'Async' to the group, detaches it when the Async is done.
  belongsTo' :: (MonadIO m) => a -> Async () -> Int -> m ()
  -- | Cancels every 'Async' currently in the group
  cancel' :: (MonadIO m) => a -> Int -> m ()
