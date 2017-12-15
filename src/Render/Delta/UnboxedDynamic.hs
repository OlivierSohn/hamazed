-- This is copied from https://hackage.haskell.org/package/dynamic-mvector-0.1.0.5/docs/src/Data-Vector-Mutable-Dynamic.html
-- with following modifications:
--    - slightly adapted to use unboxed vectors
--    - add accessUnderlying function
--    - fixed pedantic warnings

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A wrapper around MVector that enables pushing, popping and extending.

module Render.Delta.UnboxedDynamic(
      STVector, IOVector,
      -- * Initialization
      new, replicate, unsafeNew, unsafeReplicate,

      -- * Accessing
      read, write, readFront, readBack,
      unsafeRead, unsafeWrite, unsafeReadFront, unsafeReadBack, set,

      -- * Conversion
      freeze, thaw, frozen, unsafeFreeze, unsafeThaw, unsafeFrozen, accessUnderlying,

      -- * Length information
      length, null, capacity,

      -- * Copying
      clone, copy, move, unsafeCopy, unsafeMove,

      -- * Modification
      clear, reserve, unsafeReserve, trim, pushBack, popBack, unsafePopBack, extend
      ) where


import Prelude hiding (read, length, replicate, null)
import Data.Data (Typeable)

import Control.Monad
import Control.Monad.ST

import Control.Monad.Primitive
import Data.Primitive.MutVar

import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V


-- | Mutable vector with dynamic behaviour living in the ST or IO monad.
newtype MVector s a = MVector (MutVar s (MVectorData s a)) deriving (Typeable)

type IOVector = MVector RealWorld
type STVector = MVector

data MVectorData s a = MVectorData {
    _size     ::  {-# UNPACK #-} !Int,
    _data     ::                 !(MV.MVector s a)}
    deriving (Typeable)

-- amount of extra reserved space when creating a new vector
newReserve :: Int
newReserve = 5

-- | O(1) access to the underlying vector
accessUnderlying :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m (MV.MVector (PrimState m) a)
accessUnderlying (MVector v') = do
  MVectorData sz v <- readMutVar v'
  return $ MV.take sz v
{-# INLINABLE accessUnderlying #-}

-- | Create an immutable copy of the vector.
freeze :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m (V.Vector a)
freeze (MVector v') = do
    MVectorData s v <- readMutVar v'
    V.freeze (MV.unsafeSlice 0 s v)
{-# INLINABLE freeze #-}

-- | Convert a mutable vector to an immutable one without copying. The mutable vector shouldn't be accessed afterwards.
unsafeFreeze :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m (V.Vector a)
unsafeFreeze (MVector v') = do
    MVectorData s v <- readMutVar v'
    V.unsafeFreeze (MV.unsafeSlice 0 s v)
{-# INLINABLE unsafeFreeze #-}

-- | Create a mutable copy from an immutable vector.
thaw :: (PrimMonad m, V.Unbox a) => V.Vector a -> m (MVector (PrimState m) a)
thaw v' = do
    vdat <- V.thaw v'
    v <- newMutVar (MVectorData (V.length v') vdat)
    return (MVector v)
{-# INLINABLE thaw #-}

-- | Convert an immutable vector to a mutable one wihout copying.
unsafeThaw :: (PrimMonad m, V.Unbox a) => V.Vector a -> m (MVector (PrimState m) a)
unsafeThaw v' = do
    vdat <- V.unsafeThaw v'
    v <- newMutVar (MVectorData (V.length v') vdat)
    return (MVector v)
{-# INLINABLE unsafeThaw #-}

-- | Number of elements in the vector.
length :: PrimMonad m => MVector (PrimState m) a -> m Int
length (MVector v) = liftM _size (readMutVar v)
{-# INLINABLE length #-}

-- | Number of elements that the vector currently have reserved space for.
capacity :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m Int
capacity (MVector v) = liftM (MV.length . _data) (readMutVar v)
{-# INLINABLE capacity #-}

-- | Check whether the vector is empty.
null :: PrimMonad m => MVector (PrimState m) a -> m Bool
null (MVector v) = do
  MVectorData s _ <- readMutVar v
  return (s == 0)
{-# INLINABLE null #-}

-- | Create a new vector of given length. The elements are uninitialized and throw error upon accessing.
-- The "Int" argument must be positive.
new :: (PrimMonad m, V.Unbox a) => Int -> m (MVector (PrimState m) a)
new i = do
    v  <- MV.new (i + newReserve)
    liftM MVector $ newMutVar (MVectorData i v)
{-# INLINABLE new #-}

-- | "New" with the "Int" argument unchecked.
unsafeNew :: (PrimMonad m, V.Unbox a) => Int -> m (MVector (PrimState m) a)
unsafeNew i = do
    v  <- MV.unsafeNew (i + newReserve)
    liftM MVector $ newMutVar (MVectorData i v)
{-# INLINABLE unsafeNew #-}

-- | Returns a vector consisting of a value repeated the given times.
-- Throws an error if the "Int" argument is negative.
replicate :: (PrimMonad m, V.Unbox a) => Int -> a -> m (MVector (PrimState m) a)
replicate i a = do
    v <- MV.new i
    MV.set v a
    liftM MVector $ newMutVar (MVectorData i v)
{-# INLINABLE replicate #-}

-- | Replicate without checking the "Int" argument.
unsafeReplicate :: (PrimMonad m, V.Unbox a) => Int -> a -> m (MVector (PrimState m) a)
unsafeReplicate i a = do
    v <- MV.unsafeNew i
    MV.set v a
    liftM MVector $ newMutVar (MVectorData i v)
{-# INLINABLE unsafeReplicate #-}

-- | Read a value from a location. Preforms bounds checking.
read :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> Int -> m a
read (MVector v') i = do
    MVectorData s v <- readMutVar v'
    if (i >= s || i < 0) then
        error "Data.Vector.Mutable.Dynamic: read: index out of bounds"
    else
        MV.unsafeRead v i
{-# INLINABLE read #-}

-- | Read without  bounds checking.
unsafeRead :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> Int -> m a
unsafeRead (MVector v) i = (`MV.unsafeRead` i) . _data =<< readMutVar v
{-# INLINABLE unsafeRead #-}

-- | Write a value to a location. Performs bounds checking.
write :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> Int -> a -> m ()
write (MVector v') i a = do
    MVectorData s v <- readMutVar v'
    if (i >= s || i < 0) then
        error "Data.Vector.Mutable.Dynamic: write: index out of bounds"
    else
        MV.unsafeWrite v i a
{-# INLINABLE write #-}

-- | Write without bounds checking.
unsafeWrite :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> Int -> a -> m ()
unsafeWrite (MVector v') i a = do
    v <- readMutVar v'
    MV.unsafeWrite (_data v) i a
{-# INLINABLE unsafeWrite #-}

-- | Clear the vector of its contents, setting its length to 0.
clear :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m ()
clear (MVector var) = do
    v <- MV.unsafeNew newReserve
    writeMutVar var (MVectorData 0 v)
{-# INLINABLE clear #-}

-- | Set all the elements to a value.
set :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> a -> m ()
set (MVector v') a = do
    MVectorData s v <- readMutVar v'
    MV.set (MV.unsafeSlice 0 s v) a
{-# INLINABLE set #-}

-- | Move the contents of the right vector to the left one. Inputs must have the same length and must not overlap.
copy :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
copy (MVector v1') (MVector v2') = do
    v1 <- readMutVar v1'
    v2 <- readMutVar v2'
    MV.copy (_data v1) (_data v2)
{-# INLINABLE copy #-}

-- | Copy the contents of the right vector to the left one without checking length and overlapping.
unsafeCopy :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
unsafeCopy (MVector v1') (MVector v2') = do
    v1 <- readMutVar v1'
    v2 <- readMutVar v2'
    MV.unsafeCopy (_data v1) (_data v2)
{-# INLINABLE unsafeCopy #-}

-- | Move the contents of the right vector to the left one. The vectors must be the same length but may overlap.
move :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
move (MVector v1') (MVector v2') = do
    v1 <- readMutVar v1'
    v2 <- readMutVar v2'
    MV.move (_data v1) (_data v2)
{-# INLINABLE move #-}

-- | Move the contents of the right vector to the left one. The vectors must have the same length and may overlap.
-- Input lengths are unchecked.
unsafeMove :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
unsafeMove (MVector v1') (MVector v2') = do
    v1 <- readMutVar v1'
    v2 <- readMutVar v2'
    MV.unsafeMove (_data v1) (_data v2)
{-# INLINABLE unsafeMove #-}

-- | Create a copy from a mutable vector.
clone :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m (MVector (PrimState m) a)
clone (MVector v0) = do
    MVectorData s v <- readMutVar v0
    v' <- MV.clone v
    var <- newMutVar (MVectorData s v')
    return (MVector var)
{-# INLINABLE clone #-}

-- | Ensure that an amount of capacity is reserved in the vector. A no-op if there is already enough capacity.
-- Throws an error if the argument is negative.
reserve :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> Int -> m ()
reserve (MVector v) i = do
    MVectorData s v' <- readMutVar v
    if (i < 0) then
        error "Data.Vector.Mutable.Dynamic: reserve: negative argument"
    else if (s + i <= MV.length v') then
        return ()
    else do
        v'' <- MV.unsafeGrow v' i
        writeMutVar v (MVectorData s v'')
{-# INLINABLE reserve #-}

-- | Ensure that an amount of capacity is reserved in the vector. A no-op if there is already enough capacity.
-- The argument is unchecked.
unsafeReserve :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> Int -> m ()
unsafeReserve (MVector v) i = do
    MVectorData s v' <- readMutVar v
    if (s + i <= MV.length v') then
        return ()
    else do
        v'' <- MV.unsafeGrow v' i
        writeMutVar v (MVectorData s v'')
{-# INLINABLE unsafeReserve #-}

-- | Set reserved capacity to 0.
trim :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m ()
trim v = unsafeReserve v 0
{-# INLINABLE trim #-}

-- | Increment the size of the vector and write a value to the back.
-- Pushing to a slice will potentially overwrite the original vector's elements.
pushBack :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> a -> m ()
pushBack (MVector v) a = do
    MVectorData s v' <- readMutVar v
    if (s == MV.length v') then do
        v'' <- MV.unsafeGrow v' (s * 2 + 1)
        MV.unsafeWrite v'' s a
        writeMutVar v (MVectorData (s + 1) v'')
    else do
        MV.unsafeWrite v' s a
        writeMutVar v (MVectorData (s + 1) v')
{-# INLINABLE pushBack #-}


-- | Read the back value and remove it from the vector. Throws an error if the vector is empty.
popBack :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m a
popBack (MVector v) = do
    MVectorData s vec <- readMutVar v
    if (s <= 0) then
        error "Data.Vector.Mutable.Dynamic: popBack: empty vector"
    else do
        a <- MV.unsafeRead vec (s - 1)
        if (s < quot (MV.length vec) 2) then do
            vec' <- MV.unsafeGrow vec (s - 1)
            writeMutVar v (MVectorData (s - 1) vec')
        else
            writeMutVar v (MVectorData (s - 1) vec)
        return a
{-# INLINABLE popBack #-}

-- | Read the back value and remove it from the vector, without checking.
unsafePopBack :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m a
unsafePopBack (MVector v) = do
    MVectorData s vec <- readMutVar v
    a <- MV.unsafeRead vec (s - 1)
    if (s < quot (MV.length vec) 2) then do
        vec' <- MV.unsafeGrow vec (s - 1)
        writeMutVar v (MVectorData (s - 1) vec')
    else
        writeMutVar v (MVectorData (s - 1) vec)
    return a
{-# INLINABLE unsafePopBack #-}

-- | Read the back value.  Throws an error if the vector is empty.
readBack :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m a
readBack (MVector v') = do
    MVectorData s v <- readMutVar v'
    if (s <= 0) then
        error "Data.Vector.Mutable.Dynamic: reading the back of an empty vector"
    else
        MV.unsafeRead v (MV.length v - 1)
{-# INLINABLE readBack #-}

-- | Read the back value without checking.
unsafeReadBack :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m a
unsafeReadBack (MVector v') = do
    MVectorData _ v <- readMutVar v'
    MV.unsafeRead v (MV.length v - 1)
{-# INLINABLE unsafeReadBack #-}

-- | Read the front value. Throws an error if the vector is empty.
readFront :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m a
readFront (MVector v') = do
    MVectorData s v <- readMutVar v'
    if (s <= 0) then
        error "Data.Vector.Mutable.Dynamic: reading the front of an empty vector"
    else
        MV.unsafeRead v 0
{-# INLINABLE readFront #-}

-- | Read the front value without checking.
unsafeReadFront :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m a
unsafeReadFront (MVector v') = do
    MVectorData _ v <- readMutVar v'
    MV.unsafeRead v 0
{-# INLINABLE unsafeReadFront #-}

-- | Extend the vector on the left with the elements of the vector on right.
-- | Extending a slice will potentially overwrite the original vector's elements.
extend :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
extend (MVector a) (MVector b) = do
    MVectorData sa va <- readMutVar a
    MVectorData sb vb <- readMutVar b
    if (sa + sb > MV.length va) then do
        va' <- MV.unsafeGrow va (sa + sb)
        MV.unsafeCopy (MV.unsafeSlice sa sb va') (MV.unsafeSlice 0 sb vb)
        writeMutVar a (MVectorData (sa + sb) va')
    else do
        MV.unsafeCopy (MV.unsafeSlice sa sb va) (MV.unsafeSlice 0 sb vb)
        writeMutVar a (MVectorData (sa + sb) va)
{-# INLINABLE extend #-}

-- | Apply a function to an immutable copy of the vector.
frozen :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> (V.Vector a -> b) -> m b
frozen v f = liftM f (freeze v)
{-# INLINABLE frozen #-}

-- | Apply a function to the vector recast as immutable.
-- This is usually unsafe if we later modify the vector.
unsafeFrozen :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> (V.Vector a -> b) -> m b
unsafeFrozen v f = liftM f (unsafeFreeze v)
{-# INLINABLE unsafeFrozen #-}
