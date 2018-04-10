{-# OPTIONS_GHC -O2 #-}
-- | This module is like Imj.Data.Graph, but for undirected graphs only,
-- with a more efficient representation, where each vertex can have at most 4
-- neighbours, hence the name of the module.

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}


#include "containers.h"

module Imj.Data.UndirectedGraph (

    -- * Graphs
      Graph
    , Vertex

    -- ** Graph Properties
    , vertices
    , degree

    -- ** Graph Algorithms
    , dfs
    , dff
    , topSort
    , components
    , componentsN
    , reachable
    , path

    -- * Trees
    , module Data.Tree

    ) where


import Control.Monad.ST
import Data.Array.ST.Safe (STUArray, newArray, readArray, writeArray)
import Data.Tree (Tree(Node), Forest)
import GHC.Int(Int32)
import qualified Data.Vector as UV

-------------------------------------------------------------------------
--                                                                      -
--      Graphs
--                                                                      -
-------------------------------------------------------------------------

-- | Abstract representation of vertices.
type Vertex  = Int32
-- | Adjacency list representation of an undirected graph, mapping each vertex to its
-- list of successors. When constructing the graph, please make sure that each undirected
-- edge is represented by two directed edges, one bein the inverse of the other.
type Graph = UV.Vector ([] Int32)
-- | The bounds of an @Array@.
type Bounds  = (Vertex, Vertex)

-- | Returns the list of vertices in the graph.
--
-- ==== __Examples__
--
-- > vertices (buildG (0,-1) []) == []
--
-- > vertices (buildG (0,2) [(0,1),(1,2)]) == [0,1,2]
vertices :: Graph -> [Vertex]
vertices g = [0.. fromIntegral $ UV.length g -1]

-- | Returns the list of edges in the graph.
--
-- ==== __Examples__
--
-- > edges (buildG (0,-1) []) == []
--
-- > edges (buildG (0,2) [(0,1),(1,2)]) == [(0,1),(1,2)]
--edges    :: Graph -> [Edge]
--edges g   = [ (v, w) | v <- vertices g, w <- g!v]

-- | A table of the count of edges for each node.
--
-- ==== __Examples__
--
-- > outdegree (buildG (0,-1) []) == array (0,-1) []
--
-- > outdegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,1),(1,1),(2,0)]
degree :: Graph -> UV.Vector Int32
-- This is bizarrely lazy. We build an array filled with thunks, instead
-- of actually calculating anything. This is the historical behavior, and I
-- suppose someone *could* be relying on it, but it might be worth finding
-- out. Note that we *can't* be so lazy with indegree.
degree  = UV.map (fromIntegral . length)

-------------------------------------------------------------------------
--                                                                      -
--      Depth first search
--                                                                      -
-------------------------------------------------------------------------

-- | A spanning forest of the graph, obtained from a depth-first search of
-- the graph starting from each vertex in an unspecified order.
dff          :: Graph -> Forest Vertex
dff g         = dfs g (vertices g)

dffN          :: Int32 -> Graph -> Forest Vertex
dffN n g       = dfs' (Just n) g (vertices g)

-- | A spanning forest of the part of the graph reachable from the listed
-- vertices, obtained from a depth-first search of the graph starting at
-- each of the listed vertices in order.
dfs          :: Graph -> [Vertex] -> Forest Vertex
dfs           = dfs' Nothing

dfs'          :: Maybe Int32 -> Graph -> [Vertex] -> Forest Vertex
dfs' n g vs   = prune (0, fromIntegral $ UV.length g -1) n (map (generate g) vs)

generate     :: Graph -> Vertex -> Tree Vertex
generate g v  = Node v (map (generate g) (UV.unsafeIndex g $ fromIntegral v))

prune        :: Bounds -> Maybe Int32 -> Forest Vertex -> Forest Vertex
prune bnds mayCount ts = run bnds (maybe chop chopTakeN mayCount ts)

-- Same as 'chop', except that no more than n first-level forests
-- are computed.
chopTakeN         :: Int32 -> Forest Vertex -> SetM s (Forest Vertex)
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

newtype SetM s a = SetM { runSetM :: STUArray s Vertex Bool -> ST s a }

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

run          :: Bounds -> (forall s. SetM s a) -> a
run bnds act  = runST (newArray bnds False >>= runSetM act)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> readArray m v

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> writeArray m v True

-------------------------------------------------------------------------
--                                                                      -
--      Algorithms
--                                                                      -
-------------------------------------------------------------------------

------------------------------------------------------------
-- Algorithm 1: depth first search numbering
------------------------------------------------------------

preorder' :: Tree a -> [a] -> [a]
preorder' (Node a ts) = (a :) . preorderF' ts

preorderF' :: Forest a -> [a] -> [a]
preorderF' ts = foldr (.) id $ map preorder' ts

preorderF :: Forest a -> [a]
preorderF ts = preorderF' ts []

------------------------------------------------------------
-- Algorithm 2: topological sorting
------------------------------------------------------------

postorder :: Tree a -> [a] -> [a]
postorder (Node a ts) = postorderF ts . (a :)

postorderF   :: Forest a -> [a] -> [a]
postorderF ts = foldr (.) id $ map postorder ts

postOrd :: Graph -> [Vertex]
postOrd g = postorderF (dff g) []

-- | A topological sort of the graph.
-- The order is partially specified by the condition that a vertex /i/
-- precedes /j/ whenever /j/ is reachable from /i/ but not vice versa.
topSort      :: Graph -> [Vertex]
topSort       = reverse . postOrd

------------------------------------------------------------
-- Algorithm 3: connected components
------------------------------------------------------------

-- | The connected components of a graph.
-- Two vertices are connected if there is a path between them, traversing
-- edges in either direction.
components   :: Graph -> Forest Vertex
components    = dff

-- | Same as 'components' except that the search stops when a given count
-- of connected components are found.
componentsN  :: Int32 -> Graph -> Forest Vertex
componentsN n = dffN n


------------------------------------------------------------
-- Algorithm 6: Finding reachable vertices
------------------------------------------------------------

-- | Returns the list of vertices reachable from a given vertex.
--
-- ==== __Examples__
--
-- > reachable (buildG (0,0) []) 0 == [0]
--
-- > reachable (buildG (0,2) [(0,1), (1,2)]) 0 == [0,1,2]
reachable :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dfs g [v])

-- | Returns @True@ if the second vertex reachable from the first.
--
-- ==== __Examples__
--
-- > path (buildG (0,0) []) 0 0 == True
--
-- > path (buildG (0,2) [(0,1), (1,2)]) 0 2 == True
--
-- > path (buildG (0,2) [(0,1), (1,2)]) 2 0 == False
path :: Graph -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g v)
