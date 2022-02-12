{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Control.Concurrent.AsyncGroups.Impl
    ( AsyncGroupsImpl(..)
    -- * reexport
    , AsyncGroups(..)
    ) where

import           Imj.Prelude

import           Control.Concurrent.Async (Async, cancel, wait)
import qualified Control.Concurrent.MVar as Lazy(MVar, modifyMVar_) -- not using strict version, because Async misses NFData.

import qualified Data.Set as Set
import           Data.Set(Set)
import qualified Data.IntMap.Strict as Map
import           Data.IntMap.Strict(IntMap)

import           Imj.Control.Concurrent.AsyncGroups.Class

newtype AsyncGroupsImpl = AsyncGroupsImpl (Lazy.MVar (IntMap (Set (Async ()))))

instance AsyncGroups AsyncGroupsImpl where
  belongsTo' (AsyncGroupsImpl m) a w =
    liftIO $ do
      addRequestAsync m a w
      void $ wait a
      removeRequestAsync m a w

  cancel' (AsyncGroupsImpl m) = liftIO . releaseRequestResources m

  {-# INLINABLE belongsTo' #-}
  {-# INLINABLE cancel' #-}

{-# INLINE addRequestAsync #-}
addRequestAsync :: Lazy.MVar (IntMap (Set (Async ()))) -> Async () -> Int -> IO ()
addRequestAsync r a wid =
  Lazy.modifyMVar_ r $ return . ($!) Map.alter alt wid
 where
  alt = Just . Set.insert a . fromMaybe Set.empty

{-# INLINE removeRequestAsync #-}
removeRequestAsync :: Lazy.MVar (IntMap (Set (Async ()))) -> Async () -> Int -> IO ()
removeRequestAsync r a wid = Lazy.modifyMVar_ r $ return . ($!) Map.alter alt wid
 where
  alt = maybe
    Nothing
    (\set ->
      let s = Set.delete a set
      in bool (Just s) Nothing $ Set.null s)

{-# INLINABLE releaseRequestResources #-}
releaseRequestResources :: Lazy.MVar (IntMap (Set (Async ()))) -> Int -> IO ()
releaseRequestResources r wid = Lazy.modifyMVar_ r $ \m -> do
  let (e, m') = Map.updateLookupWithKey (\_ _ -> Nothing) wid m
  maybe
    (return ())
    (mapM_ cancel)
    e
  return $! m'
