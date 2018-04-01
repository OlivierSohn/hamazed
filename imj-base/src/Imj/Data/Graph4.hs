{-# OPTIONS_GHC -O2 #-}
-- | This module is like Imj.Data.Graph, but for undirected graphs only,
-- with a more efficient representation, where each vertex can have at most 4
-- neighbours, hence the name of the module.
--
-- Keys are Int between 0 and 2^15.

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

#include "containers.h"

module Imj.Data.Graph4 (

    -- * Graphs
      Graph4
    , List4(..)

    -- ** Graph4 Construction
    , graphFromEdges
    , graphFromSortedEdges
    , graphFromEdgesWithConsecutiveKeys
    , graphFromEdgesWithConsecutiveAscKeys

    -- ** Graph Properties
    , vertices
    , outdegree
    , indegree

    -- ** Graph Algorithms
    , dfs
    , dff
    , topSort
    , components
    , componentsN
    , bcc
    , reachable
    , path


    -- * Trees
    , module Data.Tree

    ) where

import           Data.Bits(shiftL, shiftR, (.|.))
import           Data.Word(Word64, Word32, Word16)
import           Data.Vector.Unboxed.Deriving(derivingUnbox)

import Control.Monad.ST
import Data.Array.ST.Safe (STUArray, newArray, readArray, writeArray)
import Data.Tree (Tree(Node), Forest)

-- std interfaces
import Data.Maybe
import Data.Array
import qualified Data.Array.Unboxed as UA
import Data.Array.Unboxed ( UArray )
import Data.List
import GHC.Generics (Generic)

-- | Abstract representation of vertices.
type Vertex  = Int

newtype List4 = List4 ([] Vertex)
  deriving(Generic)
derivingUnbox "List4"
    [t| List4 -> Word64 |]
    [| encodeList4 |]
    [| decodeList4 |]

{-# INLINE encodeList4 #-}
encodeList4 :: List4 -> Word64
encodeList4 (List4 keys) = case map fromIntegral keys of
  []        -> 0xFFFFFFFFFFFFFFFF
  [i]       -> 0xFFFFFFFFFFFF0000 .|.  i
  [i,j]     -> 0xFFFFFFFF00000000 .|. (i + j `shiftL` 16)
  [i,j,k]   -> 0xFFFF000000000000 .|. (i + j `shiftL` 16 + k `shiftL` 32)
  [i,j,k,l] ->                         i + j `shiftL` 16 + k `shiftL` 32 + l `shiftL` 48
  _ -> error $ "List4 cannot have more than 4 elements : " ++ show keys

{-# INLINE decodeList4 #-}
decodeList4 :: Word64 -> List4
decodeList4 w = List4 $ map fromIntegral keys
 where
  keys
   | i == 0xFFFF = []
   | j == 0xFFFF = [i]
   | k == 0xFFFF = [i,j]
   | l == 0xFFFF = [i,j,k]
   | otherwise   = [i,j,k,l]

  lows = fromIntegral w :: Word32
  highs = fromIntegral (w `shiftR` 32) :: Word32
  i = fromIntegral lows :: Word16
  j = fromIntegral (lows `shiftR` 16) :: Word16
  k = fromIntegral highs :: Word16
  l = fromIntegral (highs `shiftR` 16) :: Word16

-------------------------------------------------------------------------
--                                                                      -
--      Strongly Connected Components
--                                                                      -
-------------------------------------------------------------------------

-- | Strongly connected component.
data SCC vertex = AcyclicSCC vertex     -- ^ A single vertex that is not
                                        -- in any cycle.
                | CyclicSCC  [vertex]   -- ^ A maximal set of mutually
                                        -- reachable vertices.
#if __GLASGOW_HASKELL__ >= 802
  deriving ( Eq   -- ^ @since 0.5.9
           , Show -- ^ @since 0.5.9
           , Read -- ^ @since 0.5.9
           )
#else
  deriving (Eq, Show, Read)
#endif

-------------------------------------------------------------------------
--                                                                      -
--      Graphs
--                                                                      -
-------------------------------------------------------------------------

-- | Adjacency list representation of an undirected graph, mapping each vertex to its
-- list of successors. When constructing the graph, please make sure that each undirected
-- edge is represented by two directed edges, one bein the inverse of the other.
type Graph4 = Array Int List4
-- | The bounds of an @Array@.
type Bounds  = (Vertex, Vertex)

#if !USE_UNBOXED_ARRAYS
type UArray i a = Array i a
#endif

-- | Returns the list of vertices in the graph.
--
-- ==== __Examples__
--
-- > vertices (buildG (0,-1) []) == []
--
-- > vertices (buildG (0,2) [(0,1),(1,2)]) == [0,1,2]
vertices :: Graph4 -> [Vertex]
vertices  = indices

-- | Returns the list of edges in the graph.
--
-- ==== __Examples__
--
-- > edges (buildG (0,-1) []) == []
--
-- > edges (buildG (0,2) [(0,1),(1,2)]) == [(0,1),(1,2)]
--edges    :: Graph4 -> [Edge]
--edges g   = [ (v, w) | v <- vertices g, w <- g!v]

-- | A table of the count of edges from each node.
--
-- ==== __Examples__
--
-- > outdegree (buildG (0,-1) []) == array (0,-1) []
--
-- > outdegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,1),(1,1),(2,0)]
outdegree :: Graph4 -> Array Vertex Int
-- This is bizarrely lazy. We build an array filled with thunks, instead
-- of actually calculating anything. This is the historical behavior, and I
-- suppose someone *could* be relying on it, but it might be worth finding
-- out. Note that we *can't* be so lazy with indegree.
outdegree  = fmap (\(List4 l) -> length l)

-- | A table of the count of edges into each node.
--
-- ==== __Examples__
--
-- > indegree (buildG (0,-1) []) == array (0,-1) []
--
-- > indegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,0),(1,1),(2,1)]
indegree :: Graph4 -> Array Vertex Int
indegree g = accumArray (+) 0 (bounds g) [(v, 1) | (_, outs) <- assocs g, v <- case outs of (List4 l) -> l]


-- | Build a graph from a list of nodes uniquely identified by keys,
-- with a list of keys of nodes this node should have edges to.
--
-- Time complexity : O( (V+E) * log V )
-- Memory complexity : O( V+E )
--
-- This function takes an adjacency list representing a graph with vertices of
-- type @key@ labeled by values of type @node@ and produces a @Graph4@-based
-- representation of that list. The @Graph4@ result represents the /shape/ of the
-- graph, and the functions describe a) how to retrieve the label and adjacent
-- vertices of a given vertex, and b) how to retrieve a vertex given a key.
--
-- @(graph, nodeFromVertex, vertexFromKey) = graphFromEdges edgeList@
--
-- * @graph :: Graph4@ is the raw, array based adjacency list for the graph.
-- * @nodeFromVertex :: Vertex -> (node, key, [key])@ returns the node
--   associated with the given 0-based @Int@ vertex; see /warning/ below.
-- * @vertexFromKey :: key -> Maybe Vertex@ returns the @Int@ vertex for the
--   key if it exists in the graph, @Nothing@ otherwise.
--
-- To safely use this API you must either extract the list of vertices directly
-- from the graph or first call @vertexFromKey k@ to check if a vertex
-- corresponds to the key @k@. Once it is known that a vertex exists you can use
-- @nodeFromVertex@ to access the labelled node and adjacent vertices. See below
-- for examples.
--
-- Note: The out-list may contain keys that don't correspond to nodes of the
-- graph; they are ignored.
--
-- Warning: The @nodeFromVertex@ function will cause a runtime exception if the
-- given @Vertex@ does not exist.
--
-- ==== __Examples__
--
-- An empty graph.
--
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges []
-- > graph = array (0,-1) []
--
-- A graph where the out-list references unspecified nodes (@'c'@), these are
-- ignored.
--
-- > (graph, _, _) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c'])]
-- > array (0,1) [(0,[1]),(1,[])]
--
--
-- A graph with 3 vertices: ("a") -> ("b") -> ("c")
--
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]
-- > graph == array (0,2) [(0,[1]),(1,[2]),(2,[])]
-- > nodeFromVertex 0 == ("a",'a',"b")
-- > vertexFromKey 'a' == Just 0
--
-- Get the label for a given key.
--
-- > let getNodePart (n, _, _) = n
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]
-- > getNodePart . nodeFromVertex <$> vertexFromKey 'a' == Just "A"
--
graphFromEdges
        :: [(node, Int, List4)]
        -> (Graph4, Vertex -> (node, Int, List4), Int -> Maybe Vertex)
        -- ^ Time complexities of lookup functions are O(1) for the first one
        -- and O(log V) for the second one.
graphFromEdges
  = graphFromSortedEdges . sortBy (\(_,k1,_) (_,k2,_) -> compare k1 k2)

graphFromSortedEdges
        :: [(node, Int, List4)]
        -> (Graph4, Vertex -> (node, Int, List4), Int -> Maybe Vertex)
        -- ^ Time complexities of lookup functions are O(1) for the first one
        -- and O(log V) for the second one.
graphFromSortedEdges sorted_edges
  = (graph, \v -> vertex_map ! v, key_vertex)
  where
    max_v           = length sorted_edges - 1
    bounds0         = (0,max_v) :: (Vertex, Vertex)
    edges1          = zip [0..] sorted_edges

    -- 'mapMaybe key_vertex ks' induces the O(E * log V) time complexity
    graph           = array bounds0 [(,) v (List4 $ mapMaybe key_vertex ks) | (,) v (_, _, List4 ks) <- edges1]
    key_map         = array bounds0 [(,) v k                       | (,) v (_,    k, _ ) <- edges1]
    vertex_map      = array bounds0 edges1

    -- key_vertex :: key -> Maybe Vertex
    --  returns Nothing for non-interesting vertices
    key_vertex k   = findVertex 0 max_v -- Binary search on vertices: time complexity O(log V)
                   where
                     findVertex a b | a > b
                              = Nothing
                     findVertex a b = case compare k (key_map ! mid) of
                                   LT -> findVertex a (mid-1)
                                   EQ -> Just mid
                                   GT -> findVertex (mid+1) b
                              where
                                mid = a + (b - a) `div` 2


-- | Same as 'graphFromEdges' except the keys are expected to be 'Integral'
-- and consecutive.
--
-- Time complexity : O(E + (V*log V))
-- Memory complexity : O(V+E)
graphFromEdgesWithConsecutiveKeys
        :: [(node, Int, List4)]
        -- ^ The keys that are the middle elements of the tuples are
        -- expected to be a set of consecutive values.
        -> (Graph4, Vertex -> (node, Int, List4), Int -> Maybe Vertex)
        -- ^ Both lookup functions have a time complexity of O(1).
graphFromEdgesWithConsecutiveKeys
  = graphFromEdgesWithConsecutiveAscKeys . sortBy (\(_,k1,_) (_,k2,_) -> compare k1 k2)


-- | Same as 'graphFromEdges' except the keys are expected to be 'Integral'
-- consecutive and strictly ascending.
--
-- Time complexity : O(V+E)
-- Memory complexity : O(V+E)
graphFromEdgesWithConsecutiveAscKeys
        :: [(node, Int, List4)]
        -- ^ The keys that are the middle elements of the tuples are
        -- expected to be strictly ascending and consecutive (i.e with no gaps
        -- between them). More formaly, if the first key is @k@,
        -- the i-th key, is expected to be @k@ + i. This condition is not checked
        -- by the function. Violation of this condition results in unexpected behaviour.
        -> (Graph4, Vertex -> (node, Int, List4), Int -> Maybe Vertex)
        -- ^ Both lookup functions have a time complexity of O(1).
graphFromEdgesWithConsecutiveAscKeys edges0
  = (graph, \v -> vertex_map ! v, key_vertex)
  where
    may_min_k = case edges0 of
      [] -> Nothing
      (_,k,_):_ -> Just k

    max_v           = length edges0 - 1
    bounds0         = (0,max_v) :: (Vertex, Vertex)
    edges1          = zip [0..] edges0

    graph           = array bounds0 [(,) v (List4 $ mapMaybe key_vertex ks) | (,) v (_, _, List4 ks) <- edges1]
    vertex_map      = array bounds0 edges1

    -- key_vertex :: key -> Maybe Vertex
    key_vertex k = maybe Nothing kv may_min_k -- O(1) time complexity
     where
      kv min_k
        | v > max_v = Nothing
        | v < 0 = Nothing
        | otherwise = Just v
       where
        v = fromIntegral $ k - min_k


-------------------------------------------------------------------------
--                                                                      -
--      Depth first search
--                                                                      -
-------------------------------------------------------------------------

-- | A spanning forest of the graph, obtained from a depth-first search of
-- the graph starting from each vertex in an unspecified order.
dff          :: Graph4 -> Forest Vertex
dff g         = dfs g (vertices g)

dffN          :: Int -> Graph4 -> Forest Vertex
dffN n g       = dfs' (Just n) g (vertices g)

-- | A spanning forest of the part of the graph reachable from the listed
-- vertices, obtained from a depth-first search of the graph starting at
-- each of the listed vertices in order.
dfs          :: Graph4 -> [Vertex] -> Forest Vertex
dfs           = dfs' Nothing

dfs'          :: Maybe Int -> Graph4 -> [Vertex] -> Forest Vertex
dfs' n g vs   = prune (bounds g) n (map (generate g) vs)

generate     :: Graph4 -> Vertex -> Tree Vertex
generate g v  = Node v (map (generate g) (case g!v of (List4 l) -> l))

prune        :: Bounds -> Maybe Int -> Forest Vertex -> Forest Vertex
prune bnds mayCount ts = run bnds (maybe chop chopTakeN mayCount ts)

-- Same as 'chop', except that no more than n first-level forests
-- are computed.
chopTakeN         :: Int -> Forest Vertex -> SetM s (Forest Vertex)
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

-- A monad holding a set of vertices visited so far.
#if USE_ST_MONAD

-- Use the ST monad if available, for constant-time primitives.

#if USE_UNBOXED_ARRAYS
newtype SetM s a = SetM { runSetM :: STUArray s Vertex Bool -> ST s a }
#else
newtype SetM s a = SetM { runSetM :: STArray  s Vertex Bool -> ST s a }
#endif

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

#else /* !USE_ST_MONAD */

-- Portable implementation using IntSet.

newtype SetM s a = SetM { runSetM :: IntSet -> (a, IntSet) }

instance Monad (SetM s) where
    return x     = SetM $ \s -> (x, s)
    SetM v >>= f = SetM $ \s -> case v s of (x, s') -> runSetM (f x) s'

instance Functor (SetM s) where
    f `fmap` SetM v = SetM $ \s -> case v s of (x, s') -> (f x, s')
    {-# INLINE fmap #-}

instance Applicative (SetM s) where
    pure x = SetM $ \s -> (x, s)
    {-# INLINE pure #-}
    SetM f <*> SetM v = SetM $ \s -> case f s of (k, s') -> case v s' of (x, s'') -> (k x, s'')
    {-# INLINE (<*>) #-}

run          :: Bounds -> SetM s a -> a
run _ act     = fst (runSetM act Set.empty)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> (Set.member v m, m)

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> ((), Set.insert v m)

#endif /* !USE_ST_MONAD */

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

tabulate        :: Bounds -> [Vertex] -> UArray Vertex Int
tabulate bnds vs = UA.array bnds (zipWith (flip (,)) [1..] vs)
-- Why zipWith (flip (,)) instead of just using zip with the
-- arguments in the other order? We want the [1..] to fuse
-- away, and these days that only happens when it's the first
-- list argument.

preArr          :: Bounds -> Forest Vertex -> UArray Vertex Int
preArr bnds      = tabulate bnds . preorderF

------------------------------------------------------------
-- Algorithm 2: topological sorting
------------------------------------------------------------

postorder :: Tree a -> [a] -> [a]
postorder (Node a ts) = postorderF ts . (a :)

postorderF   :: Forest a -> [a] -> [a]
postorderF ts = foldr (.) id $ map postorder ts

postOrd :: Graph4 -> [Vertex]
postOrd g = postorderF (dff g) []

-- | A topological sort of the graph.
-- The order is partially specified by the condition that a vertex /i/
-- precedes /j/ whenever /j/ is reachable from /i/ but not vice versa.
topSort      :: Graph4 -> [Vertex]
topSort       = reverse . postOrd

------------------------------------------------------------
-- Algorithm 3: connected components
------------------------------------------------------------

-- | The connected components of a graph.
-- Two vertices are connected if there is a path between them, traversing
-- edges in either direction.
components   :: Graph4 -> Forest Vertex
components    = dff

-- | Same as 'components' except that the search stops when a given count
-- of connected components are found.
componentsN  :: Int -> Graph4 -> Forest Vertex
componentsN n = dffN n


------------------------------------------------------------
-- Algorithm 5: Classifying edges
------------------------------------------------------------

{-
XXX unused code

tree              :: Bounds -> Forest Vertex -> Graph4
tree bnds ts       = buildG bnds (concat (map flat ts))
 where flat (Node v ts') = [ (v, w) | Node w _us <- ts' ]
                        ++ concat (map flat ts')

back              :: Graph4 -> Table Int -> Graph4
back g post        = mapT select g
 where select v ws = [ w | w <- ws, post!v < post!w ]

cross             :: Graph4 -> Table Int -> Table Int -> Graph4
cross g pre post   = mapT select g
 where select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward           :: Graph4 -> Graph4 -> Table Int -> Graph4
forward g tree' pre = mapT select g
 where select v ws = [ w | w <- ws, pre!v < pre!w ] \\ tree' ! v

mapT    :: (Vertex -> a -> b) -> Array Vertex a -> Array Vertex b
mapT f t = array (bounds t) [ (,) v (f v (t!v)) | v <- indices t ]
-}

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
reachable :: Graph4 -> Vertex -> [Vertex]
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
path :: Graph4 -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g v)

------------------------------------------------------------
-- Algorithm 7: Biconnected components
------------------------------------------------------------

-- | The biconnected components of a graph.
-- An undirected graph is biconnected if the deletion of any vertex
-- leaves it connected.
bcc :: Graph4 -> Forest [Vertex]
bcc g = (concat . map bicomps . map (do_label g dnum)) forest
 where forest = dff g
       dnum   = preArr (bounds g) forest

do_label :: Graph4 -> UArray Vertex Int -> Tree Vertex -> Tree (Vertex,Int,Int)
do_label g dnum (Node v ts) = Node (v, dnum UA.! v, lv) us
 where us = map (do_label g dnum) ts
       lv = minimum ([dnum UA.! v] ++ [dnum UA.! w | w <- case g!v of (List4 l) -> l]
                     ++ [lu | Node (_,_,lu) _ <- us])

bicomps :: Tree (Vertex,Int,Int) -> Forest [Vertex]
bicomps (Node (v,_,_) ts)
      = [ Node (v:vs) us | (_,Node vs us) <- map collect ts]

collect :: Tree (Vertex,Int,Int) -> (Int, Tree [Vertex])
collect (Node (v,dv,lv) ts) = (lv, Node (v:vs) cs)
 where collected = map collect ts
       vs = concat [ ws | (lw, Node ws _) <- collected, lw<dv]
       cs = concat [ if lw<dv then us else [Node (v:ws) us]
                        | (lw, Node ws us) <- collected ]
