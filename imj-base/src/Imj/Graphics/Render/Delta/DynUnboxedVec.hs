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
* fixed new / unsafeNew (the size was equal to the capacity instead of zero)
* Removed functions that I don't use and won't have time to support

-}


module Imj.Graphics.Render.Delta.DynUnboxedVec(
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

import           Data.Data
                    (Typeable)
import           Data.Vector.Algorithms.Intro
                    (sort) -- unstable sort

import           Control.Monad.Primitive
                    (RealWorld, PrimMonad, PrimState)
import           Data.Primitive.MutVar
                    (MutVar, readMutVar, newMutVar, writeMutVar)

import qualified Data.Vector.Unboxed.Mutable as MV
                    (MVector, take, length, new, unsafeRead,
                     unsafeGrow, unsafeWrite)
import qualified Data.Vector.Unboxed as V
                    (Unbox)


-- | Mutable vector with dynamic behaviour living in the ST or IO monad.
newtype MVector s a = MVector (MutVar s (MVectorData s a)) deriving (Typeable)

type IOVector = MVector RealWorld
type STVector = MVector

data MVectorData s a = MVectorData {
    _mVectorDatasize   ::  {-# UNPACK #-} !Int,
    _mVectorDataBuffer ::                 !(MV.MVector s a)}
    deriving (Typeable)

-- | O(1) access to the underlying vector
{-# INLINABLE accessUnderlying #-}
accessUnderlying :: (PrimMonad m, V.Unbox a)
                 => MVector (PrimState m) a
                 -> m (MV.MVector (PrimState m) a)
accessUnderlying (MVector v') =
  readMutVar v'
    >>=
      \(MVectorData sz v) -> return $ MV.take sz v


-- | O(N*log(N)) unstable sort.
unstableSort :: (PrimMonad m, V.Unbox a, Ord a)
             => MVector (PrimState m) a
             -> m ()
unstableSort v =
  accessUnderlying v
    >>= sort
{-# INLINABLE unstableSort #-}

-- | Number of elements in the vector.
length :: PrimMonad m
       => MVector (PrimState m) a
       -> m Int
length (MVector v) =
  readMutVar v
    >>= \(MVectorData sz _) -> return sz
{-# INLINABLE length #-}

-- | Number of elements that the vector currently has reserved space for.
capacity :: (PrimMonad m, V.Unbox a)
         => MVector (PrimState m) a
         -> m Int
capacity (MVector v) =
  readMutVar v
    >>= \(MVectorData _ d) -> return $ MV.length d
{-# INLINABLE capacity #-}

-- | Create a vector with a given capacity.
new :: (PrimMonad m, V.Unbox a)
    => Int -- ^ Capacity, must be positive
    -> m (MVector (PrimState m) a)
new i =
    MV.new i
      >>=
        \v -> MVector <$> newMutVar (MVectorData 0 v)
{-# INLINABLE new #-}

-- | Read by index. Performs bounds checking.
read :: (PrimMonad m, V.Unbox a)
     => MVector (PrimState m) a
     -> Int
     -> m a
read (MVector v') i =
  readMutVar v'
    >>=
      \(MVectorData s v) ->
          if i >= s || i < 0
            then
              error "Data.Vector.Mutable.Dynamic: read: index out of bounds"
            else
              MV.unsafeRead v i
{-# INLINABLE read #-}

-- | Read by index without bounds checking.
unsafeRead :: (PrimMonad m, V.Unbox a)
           => MVector (PrimState m) a
           -> Int
           -> m a
unsafeRead (MVector v) i =
  readMutVar v
    >>=
      \(MVectorData _ d) -> d `MV.unsafeRead` i
{-# INLINABLE unsafeRead #-}

-- | Clear the vector, set length to 0.
--
-- Does not reallocate, capacity is unchanged.
clear :: PrimMonad m
      => MVector (PrimState m) a
      -> m ()
clear (MVector v) =
  readMutVar v
    >>=
      \(MVectorData _ d) -> writeMutVar v (MVectorData 0 d)
{-# INLINABLE clear #-}

-- | Increment the size of the vector and write a value to the back.
pushBack :: (PrimMonad m, V.Unbox a)
         => MVector (PrimState m) a
         -> a
         -> m ()
pushBack (MVector v) a =
  readMutVar v
    >>=
      \(MVectorData s v') ->
          if s == MV.length v'
            then do
              -- nearly double size each time.
              v'' <- MV.unsafeGrow v' (s + 1)
              MV.unsafeWrite v'' s a
              writeMutVar v (MVectorData (s + 1) v'')
            else do
              MV.unsafeWrite v' s a
              writeMutVar v (MVectorData (s + 1) v')
{-# INLINABLE pushBack #-}
