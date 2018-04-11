{-# OPTIONS_GHC -O2 #-}
-- | This module is like Imj.Data.Graph, but for undirected graphs only.

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}


#include "containers.h"

module Imj.Data.UndirectedGraph (

    -- * Graphs
      Graph
    , Vertex

    -- ** Graph Algorithms
    , components
    , componentsN

    -- * Trees
    , module Data.Tree

    ) where

import           Control.Monad.ST
import           Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as S
import           Data.Tree (Tree(Node), Forest)
import           GHC.Int(Int64)
import qualified Data.Vector as UV

-------------------------------------------------------------------------
--                                                                      -
--      Graphs
--                                                                      -
-------------------------------------------------------------------------

-- | Abstract representation of vertices.
type Vertex  = Int64
-- | Adjacency list representation of an undirected graph, mapping each vertex to its
-- list of successors. When constructing the graph, please make sure that each undirected
-- edge is represented by two directed edges, one being the inverse of the other.
type Graph = UV.Vector ([] Int64)

-------------------------------------------------------------------------
--                                                                      -
--      Depth first search
--                                                                      -
-------------------------------------------------------------------------

componentsN        :: Int64 -> Graph -> Forest Vertex
componentsN n g       = dfs' (Just n) g

-- | A spanning forest of the part of the graph reachable from the listed
-- vertices, obtained from a depth-first search of the graph starting at
-- each of the listed vertices in order.
components :: Graph -> Forest Vertex
components = dfs' Nothing

forLoop :: Int64 -> Int64 -> (Int64 -> a) -> [a]
forLoop start end f = go start
  where
    go x
      | x == end = []
      | otherwise = f x : go (x+1)


dfs'          :: Maybe Int64 -> Graph -> Forest Vertex
dfs' n g   = prune v n (forLoop 0 (fromIntegral v) (generate g))
 where v = UV.length g

generate     :: Graph -> Vertex -> Tree Vertex
generate g v  = Node v (map (generate g) (UV.unsafeIndex g $ fromIntegral v))

prune        :: Int -> Maybe Int64 -> Forest Vertex -> Forest Vertex
prune n mayCount ts = run n (maybe chop chopTakeN mayCount ts)

-- Same as 'chop', except that no more than n first-level forests
-- are computed.
chopTakeN         :: Int64 -> Forest Vertex -> SetM s (Forest Vertex)
chopTakeN 0 _     = return []
chopTakeN _ []     = return []
chopTakeN n (Node v ts : us)
              = do
                visited <- contains v
                if visited then
                  chopTakeN n us
                 else do
                  include v
                  as <- chop ts
                  bs <- chopTakeN (n-1) us
                  return (Node v as : bs)

chop         :: Forest Vertex -> SetM s (Forest Vertex)
chop []     = return []
chop (Node v ts : us)
              = do
                visited <- contains v
                if visited then
                  chop us
                 else do
                  include v
                  as <- chop ts
                  bs <- chop us
                  return (Node v as : bs)

-- Use the ST monad, for constant-time primitives.

newtype SetM s a = SetM { runSetM :: MVector s Bool -> ST s a }

instance Monad (SetM s) where
    return = pure
    {-# INLINE return #-}
    SetM v >>= f = SetM $ \s -> do { x <- v s; runSetM (f x) s }
    {-# INLINE (>>=) #-}

instance Functor (SetM s) where
    f `fmap` SetM v = SetM $ \s -> f `fmap` v s
    {-# INLINE fmap #-}

instance Applicative (SetM s) where
    pure x = SetM $ const (return x)
    {-# INLINE pure #-}
    SetM f <*> SetM v = SetM $ \s -> f s >>= (`fmap` v s)
    -- We could also use the following definition
    --   SetM f <*> SetM v = SetM $ \s -> f s <*> v s
    -- but Applicative (ST s) instance is present only in GHC 7.2+
    {-# INLINE (<*>) #-}

run          :: Int -> (forall s. SetM s a) -> a
run n act  = runST (S.replicate n False >>= runSetM act)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> S.unsafeRead m (fromIntegral v)

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> S.unsafeWrite m (fromIntegral v) True
