{-# OPTIONS_GHC -O2 #-}
-- | This module is like Imj.Data.Graph, but for undirected graphs only.

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

#include "containers.h"

module Imj.Data.UndirectedGraph (

    -- * Graphs
      Graph(..)
    , Vertex
    , Tree
    , Forest
    , foldTree
    , flatten

    -- ** Graph Algorithms
    , components
    , componentsN


    ) where

import           Control.Monad.ST
import           Data.Bits(shiftL)
import           Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as S
import           GHC.Word(Word8, Word16)
import           Data.Primitive.ByteArray(ByteArray(..), indexByteArray)

-------------------------------------------------------------------------
--                                                                      -
--      Graphs
--                                                                      -
-------------------------------------------------------------------------

-- | Abstract representation of vertices. Vertices /must/ be < 0x8000
type Vertex  = Word16
-- | Adjacency list representation of an undirected graph, mapping each vertex to its
-- list of successors (at most 4 elements, encoded as Word16).
--
-- When constructing the graph, please make sure that each undirected
-- edge is represented by two directed edges, one being the inverse of the other.
data Graph = Graph {
    _nNodes :: {-# UNPACK #-} !Word16
    -- ^ The number of nodes in the graph.
    -- Must be <= fromIntegral $ quot (sizeofByteArray _memory) SIZEOF_WORD64
  , _memory :: !ByteArray
}

-- Like Data.Tree(Tree) but strict in the value
data Tree = Node {
  _rootLabel :: {-# UNPACK #-} !Vertex, -- ^ label value
  _subForest :: Forest   -- ^ zero or more child trees
}
type Forest = [Tree]

-- | The elements of a tree in pre-order.
flatten :: Tree -> [Vertex]
flatten t = squish t []
  where
    squish (Node x ts) xs = x:Prelude.foldr squish xs ts

foldTree :: (Vertex -> [b] -> b) -> Tree -> b
foldTree f = go where
    go (Node x ts) = f x (map go ts)

-------------------------------------------------------------------------
--                                                                      -
--      Depth first search
--                                                                      -
-------------------------------------------------------------------------

componentsN        :: Word16 -> Graph -> Forest
componentsN n g       = dfs' (Just n) g

-- | A spanning forest of the part of the graph reachable from the listed
-- vertices, obtained from a depth-first search of the graph starting at
-- each of the listed vertices in order.
components :: Graph -> Forest
components = dfs' Nothing

{-# INLINE forLoop #-}
forLoop :: Word16 -> Word16 -> (Word16 -> a) -> [a]
forLoop start end f = go start
  where
    go x
      | x == end = []
      | otherwise = f x : go (x+1)


dfs'          :: Maybe Word16 -> Graph -> Forest
dfs' n (Graph sz g)   = prune sz n (forLoop 0 sz (generate g))

generate     :: ByteArray -> Vertex -> Tree
generate g v  = Node v (map4 (generate g) $ neighbours v)
 where
  neighbours :: Vertex -> Four -- TODO 3% time in profiling, we could try optimizing it.
  neighbours x =
    let base = fromIntegral $ x `shiftL` 2
        w1 = indexByteArray g base :: Word16
        w2 = indexByteArray g (base + 1) :: Word16
        w3 = indexByteArray g (base + 2) :: Word16
        w4 = indexByteArray g (base + 3) :: Word16
    in Four w1 w2 w3 w4

  map4 f (Four w1 w2 w3 w4) =
    map f $ filter (< 0x8000) [w1,w2,w3,w4]

data Four = Four {-# UNPACK #-} !Vertex {-# UNPACK #-} !Vertex {-# UNPACK #-} !Vertex {-# UNPACK #-} !Vertex

prune        :: Word16 -> Maybe Word16 -> Forest -> Forest
prune n mayCount ts = run n (maybe chop chopTakeN mayCount ts)

-- Same as 'chop', except that no more than n first-level forests
-- are computed.
chopTakeN         :: Word16 -> Forest -> SetM s Forest
chopTakeN 0 _     = return []
chopTakeN _ []     = return []
chopTakeN n (Node v ts : us)
              = do
                visited <- contains v
                if visited /= 0 then
                  chopTakeN n us
                 else do
                  include v
                  as <- chop ts
                  bs <- chopTakeN (n-1) us
                  return (Node v as : bs)

chop         :: Forest -> SetM s Forest
chop []     = return []
chop (Node v ts : us)
              = do
                visited <- contains v
                if visited /= 0 then
                  chop us
                 else do
                  include v
                  as <- chop ts
                  bs <- chop us
                  return (Node v as : bs)

-- Use the ST monad, for constant-time primitives.


newtype SetM s a = SetM { runSetM :: MVector s Word8 -> ST s a }

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

{-# INLINE run #-}
run          :: Word16 -> (forall s. SetM s a) -> a
run n act  = runST (S.replicate (fromIntegral n) 0 >>= runSetM act)

{-# INLINE contains #-}
contains     :: Vertex -> SetM s Word8
contains v    = SetM $ \ m -> S.unsafeRead m (fromIntegral v)

{-# INLINE include #-}
include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> S.unsafeWrite m (fromIntegral v) (0xFF)
