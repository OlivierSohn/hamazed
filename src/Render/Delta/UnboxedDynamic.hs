{- | A wrapper around 'Data.Vector.Unboxed.Mutable' that enables reserving,
clearing, pushing (in C++ STL vector fashion).

Modified from https://hackage.haskell.org/package/dynamic-mvector-0.1.0.5/docs/src/Data-Vector-Mutable-Dynamic.html :

* Adapted to use unboxed vectors
* Added 'accessUnderlying' to be able to use sort algorithms efficiently, without copying.
* Changed behaviour of clear, to avoid reallocation.
* Removed functions that I don't use and won't have time to support

-}

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Render.Delta.UnboxedDynamic(
        STVector
      , IOVector
      -- * Creation
      , new

      -- * Accessing elements
      , read
      , unsafeRead

      -- * Modification
      , write
      , unsafeWrite
      , clear
      , pushBack

      -- * Accessing underlying buffer
      , accessUnderlying

      -- * Length
      , length

      ) where


import Prelude hiding (read, length)
import Data.Data (Typeable)

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
    _mVectorDatasize   ::  {-# UNPACK #-} !Int,
    _mVectorDataBuffer ::                 !(MV.MVector s a)}
    deriving (Typeable)

-- | O(1) access to the underlying vector
accessUnderlying :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m (MV.MVector (PrimState m) a)
accessUnderlying (MVector v') = do
  MVectorData sz v <- readMutVar v'
  return $ MV.take sz v
{-# INLINABLE accessUnderlying #-}

-- | Number of elements in the vector.
length :: PrimMonad m => MVector (PrimState m) a -> m Int
length (MVector v) =
  readMutVar v
    >>= \(MVectorData sz _) -> return sz
{-# INLINABLE length #-}

-- | Create a vector of a given capacity.
new :: (PrimMonad m, V.Unbox a)
    => Int -- ^ Capacity, must be positive
    -> m (MVector (PrimState m) a)
new i = do
    v  <- MV.new i
    MVector <$> newMutVar (MVectorData i v)
{-# INLINABLE new #-}

-- | Read a value by index. Performs bounds checking.
read :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> Int -> m a
read (MVector v') i = do
    MVectorData s v <- readMutVar v'
    if i >= s || i < 0 then
        error "Data.Vector.Mutable.Dynamic: read: index out of bounds"
    else
        MV.unsafeRead v i
{-# INLINABLE read #-}

-- | Read without bounds checking.
unsafeRead :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> Int -> m a
unsafeRead (MVector v) i =
  readMutVar v
    >>=
      \(MVectorData _ d) -> d `MV.unsafeRead` i
{-# INLINABLE unsafeRead #-}

-- | Write a value to a location. Performs bounds checking.
write :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> Int -> a -> m ()
write (MVector v') i a = do
    MVectorData s v <- readMutVar v'
    if i >= s || i < 0 then
        error "Data.Vector.Mutable.Dynamic: write: index out of bounds"
    else
        MV.unsafeWrite v i a
{-# INLINABLE write #-}

-- | Write without bounds checking.
unsafeWrite :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> Int -> a -> m ()
unsafeWrite (MVector v') i a = do
    (MVectorData _ d) <- readMutVar v'
    MV.unsafeWrite d i a
{-# INLINABLE unsafeWrite #-}

-- | Clear the vector of its contents, setting its length to 0. Does not reallocate.
clear :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> m ()
clear (MVector var) = do
    (MVectorData _ d) <- readMutVar var
    writeMutVar var (MVectorData 0 d)
{-# INLINABLE clear #-}

-- | Increment the size of the vector and write a value to the back.
-- Pushing to a slice will potentially overwrite the original vector's elements.
pushBack :: (PrimMonad m, V.Unbox a) => MVector (PrimState m) a -> a -> m ()
pushBack (MVector v) a = do
    MVectorData s v' <- readMutVar v
    if s == MV.length v' then do
        -- nearly double size each time.
        v'' <- MV.unsafeGrow v' (s + 1)
        MV.unsafeWrite v'' s a
        writeMutVar v (MVectorData (s + 1) v'')
    else do
        MV.unsafeWrite v' s a
        writeMutVar v (MVectorData (s + 1) v')
{-# INLINABLE pushBack #-}
