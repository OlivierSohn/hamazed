{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Network.Class.AsyncGroups
    ( AsyncGroups(..)
    ) where

import           Imj.Prelude

import           Control.Concurrent.Async (Async, cancel, wait)
import qualified Control.Concurrent.MVar as Lazy(MVar, modifyMVar_) -- not using strict version, because Async misses NFData.
import           Control.Monad.IO.Class(MonadIO, liftIO)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Types

class (Ord (Key a)) => AsyncGroups a where
  type Key a
  -- | Attaches an 'Async' to the group, detaches it when the Async is done.
  belongsTo' :: (MonadIO m) => a -> Async () -> Key a -> m ()
  -- | Cancels every 'Async' currently in the group
  cancel' :: (MonadIO m) => a -> Key a -> m ()

instance (Ord k) => AsyncGroups (RequestsAsyncs k) where
  type Key (RequestsAsyncs k) = k
  belongsTo' (RequestsAsyncs m) a w =
    liftIO $ do
      addRequestAsync m a w
      void $ wait a
      removeRequestAsync m a w

  cancel' (RequestsAsyncs m) = liftIO . releaseRequestResources m

  {-# INLINABLE belongsTo' #-}
  {-# INLINABLE cancel' #-}

{-# INLINE addRequestAsync #-}
addRequestAsync :: (Ord k) => Lazy.MVar (Map k (Set (Async ()))) -> Async () -> k -> IO ()
addRequestAsync r a wid =
  Lazy.modifyMVar_ r $ return . ($!) Map.alter alt wid
 where
  alt = Just . Set.insert a . fromMaybe Set.empty

{-# INLINE removeRequestAsync #-}
removeRequestAsync :: (Ord k) => Lazy.MVar (Map k (Set (Async ()))) -> Async () -> k -> IO ()
removeRequestAsync r a wid = Lazy.modifyMVar_ r $ return . ($!) Map.alter alt wid
 where
  alt = maybe
    Nothing
    (\set ->
      let s = Set.delete a set
      in bool (Just s) Nothing $ Set.null s)

{-# INLINABLE releaseRequestResources #-}
releaseRequestResources :: (Ord k) => Lazy.MVar (Map k (Set (Async ()))) -> k -> IO ()
releaseRequestResources r wid = Lazy.modifyMVar_ r $ \m -> do
  let (e, m') = Map.updateLookupWithKey (\_ _ -> Nothing) wid m
  maybe
    (return ())
    (mapM_ cancel)
    e
  return $! m'
