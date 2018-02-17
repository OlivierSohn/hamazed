{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | A wrapper around 'Data.Vector.Unboxed.Mutable' that enables reserving,
clearing, pushing (in C++ STL vector fashion).

Modified from https://hackage.haskell.org/package/dynamic-mvector-0.1.0.5/docs/src/Data-Vector-Mutable-Dynamic.html :

* Adapted to use unboxed vectors
* Added a sort function.
* Added 'accessUnderlying' to be able to use sort algorithms efficiently, without copying.
* Changed behaviour of clear, to avoid reallocation.
* Fixed new / unsafeNew (the size was equal to the capacity instead of zero).
* Removed functions that I don't use and won't have time to support.

Unit tests : "Test.Imj.Vector"
-}


module Imj.Data.Vector.Unboxed.Mutable.Dynamic(
        STVector
      , IOVector
      -- * Creation
      , new

      -- * Access
      , read
      , unsafeRead
      , length
      , capacity

      -- * Modify
      , clear
      , pushBack
      , unstableSort
      , accessUnderlying

      ) where


import           Imj.Prelude

import           Data.Data(Typeable)
import           Data.Vector.Algorithms.Intro(sort) -- unstable sort

import           Control.Monad.Primitive(RealWorld, PrimMonad, PrimState)
import           Data.Primitive.MutVar(MutVar, readMutVar, newMutVar, writeMutVar)

import qualified Data.Vector.Unboxed.Mutable as MV(MVector, take, length, new, unsafeRead,
                                                  unsafeGrow, unsafeWrite)
import qualified Data.Vector.Unboxed as V(Unbox)


-- | Mutable vector with dynamic behaviour living in the ST or IO monad.
newtype MVector s a = MVector (MutVar s (MVectorData s a)) deriving (Typeable)

type IOVector = MVector RealWorld
type STVector = MVector

data MVectorData s a = MVectorData {
    size   ::  {-# UNPACK #-} !Int,
    buffer ::                 !(MV.MVector s a)
} deriving (Typeable)

-- | O(1) access to the underlying vector
{-# INLINABLE accessUnderlying #-}
accessUnderlying :: (PrimMonad m, V.Unbox a)
                 => MVector (PrimState m) a
                 -> m (MV.MVector (PrimState m) a)
accessUnderlying (MVector v') = readMutVar v' >>= \(MVectorData sz v) ->
  return $ MV.take sz v

-- | O(N*log(N)) unstable sort.
unstableSort :: (PrimMonad m, V.Unbox a, Ord a)
             => MVector (PrimState m) a
             -> m ()
unstableSort v =
  accessUnderlying v >>= sort
{-# INLINABLE unstableSort #-}

-- | Number of elements in the vector.
length :: PrimMonad m
       => MVector (PrimState m) a
       -> m Int
length (MVector v) =
  size <$> readMutVar v
{-# INLINABLE length #-}

-- | Number of elements that the vector currently has reserved space for.
capacity :: (PrimMonad m, V.Unbox a)
         => MVector (PrimState m) a
         -> m Int
capacity (MVector v) =
  MV.length . buffer <$> readMutVar v
{-# INLINABLE capacity #-}

-- | Create a vector with a given capacity.
new :: (PrimMonad m, V.Unbox a)
    => Int -- ^ Capacity, must be positive
    -> m (MVector (PrimState m) a)
new i =
    MV.new i >>= fmap MVector . newMutVar . MVectorData 0
{-# INLINABLE new #-}

-- | Read by index. Performs bounds checking.
read :: (PrimMonad m, V.Unbox a)
     => MVector (PrimState m) a
     -> Int
     -> m a
read (MVector v') i = readMutVar v' >>= \(MVectorData sz buf) ->
  if i >= sz || i < 0
    then
      error "Data.Vector.Mutable.Dynamic: read: index out of bounds"
    else
      MV.unsafeRead buf i
{-# INLINABLE read #-}

-- | Read by index without bounds checking.
unsafeRead :: (PrimMonad m, V.Unbox a)
           => MVector (PrimState m) a
           -> Int
           -> m a
unsafeRead (MVector v) i =
  readMutVar v >>= flip MV.unsafeRead i . buffer
{-# INLINABLE unsafeRead #-}

-- | Clear the vector, set length to 0.
--
-- Does not reallocate, capacity is unchanged.
clear :: PrimMonad m
      => MVector (PrimState m) a
      -> m ()
clear (MVector v) =
  readMutVar v >>= writeMutVar v . MVectorData 0 . buffer
{-# INLINABLE clear #-}

-- | Increment the size of the vector and write a value to the back.
pushBack :: (PrimMonad m, V.Unbox a)
         => MVector (PrimState m) a
         -> a
         -> m ()
pushBack (MVector v) a = readMutVar v >>= \(MVectorData sz buf) ->
  if sz == MV.length buf
    then do
      -- nearly double size each time.
      buf' <- MV.unsafeGrow buf (sz + 1)
      MV.unsafeWrite buf' sz a
      writeMutVar v $ MVectorData (sz + 1) buf'
    else do
      MV.unsafeWrite buf sz a
      writeMutVar v $ MVectorData (sz + 1) buf
{-# INLINABLE pushBack #-}
