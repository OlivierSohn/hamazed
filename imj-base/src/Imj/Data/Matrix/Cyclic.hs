-- adapted from https://github.com/Daniel-Diaz/matrix to use an unboxed vector
-- and allow indices rotation

{-# LANGUAGE DeriveGeneric #-}
module Imj.Data.Matrix.Cyclic (
    -- * Matrix type
    Matrix
  , nrows , ncols, rotation
    -- * Builders
  , matrix
  , rowVector
  , colVector
  , setRotation
  , unsafeSetRotation
    -- ** Special matrices
  , zero
  , identity
  , diagonal
  , permMatrix
    -- * List conversions
  , fromList , fromLists, toLists
    -- * Accessing
  , getElem , (!) , unsafeGet , safeGet, getRow, getCol, countRotations
    -- * Matrix operations
  , elementwise, elementwiseUnsafe
    -- * Matrix multiplication
    -- ** About matrix multiplication
    -- $mult

    -- ** Functions
    -- * Linear transformations
  , scaleMatrix
  -- * Reexports
  , V.Unbox
  ) where

import Prelude
-- Classes
import Control.DeepSeq
import Control.Loop (numLoop)
import Data.Maybe
import GHC.Generics (Generic)
-- Data
import qualified Data.Vector.Unboxed             as V
import qualified Data.Vector.Unboxed.Mutable     as MV

-------------------------------------------------------
-------------------------------------------------------
---- MATRIX TYPE

encode :: Int -> Int -> Int -> Int
{-# INLINE encode #-}
encode m i j = i*m + j

-- | Type of matrices.
--
--   Elements can be of any type. Rows and columns
--   are 0-indexed.
data Matrix a = M {
   nrows     :: {-# UNPACK #-} !Int -- ^ Number of rows.
 , ncols     :: {-# UNPACK #-} !Int -- ^ Number of columns.
 , rotation    :: {-# UNPACK #-} !Int -- ^ cyclic offset, always >=0 and <vector length
 , mvect     :: !(V.Vector a)        -- ^ Content of the matrix as a plain vector.
   } deriving (Generic, Show)

instance NFData (Matrix a) where
 rnf = rnf . mvect

countRotations :: (V.Unbox a) => Matrix a -> Int
countRotations (M _ _ _ v) = V.length v

setRotation :: (V.Unbox a) => Matrix a -> Int -> Matrix a
setRotation m@(M _ _ _ v) i
  | i < 0 = error "negative offset"
  | i >= V.length v = error "out of range offset"
  | otherwise = unsafeSetRotation m i

{-# INLINE unsafeSetRotation #-}
unsafeSetRotation :: Matrix a -> Int -> Matrix a
unsafeSetRotation m i = m { rotation = i }

mapMat :: (V.Unbox a, V.Unbox b)
        => (a -> b)
        -> Matrix a -> Matrix b
mapMat func (M a b c v) = M a b c $ V.map func v

-------------------------------------------------------
-------------------------------------------------------
---- BUILDERS

-- | /O(rows*cols)/. The zero matrix of the given size.
--
-- > zero n m =
-- >                 m
-- >   1 ( 0 0 ... 0 0 )
-- >   2 ( 0 0 ... 0 0 )
-- >     (     ...     )
-- >     ( 0 0 ... 0 0 )
-- >   n ( 0 0 ... 0 0 )
zero :: (Num a, V.Unbox a) =>
     Int -- ^ Rows
  -> Int -- ^ Columns
  -> Matrix a
{-# INLINE zero #-}
zero n m = M n m 0 $ V.replicate (n*m) 0

-- | /O(rows*cols)/. Generate a matrix from a generator function.
--   Example of usage:
--
-- >                                  (  1  0 -1 -2 )
-- >                                  (  3  2  1  0 )
-- >                                  (  5  4  3  2 )
-- > matrix 4 4 $ \(i,j) -> 2*i - j = (  7  6  5  4 )
matrix :: (V.Unbox a)
       => Int -- ^ Rows
       -> Int -- ^ Columns
       -> (Int -> Int -> a) -- ^ Generator function
       -> Matrix a
{-# INLINE matrix #-}
matrix n m f = M n m 0 $ V.create $ do
  v <- MV.new $ n * m
  let en = encode m
  numLoop 0 (n-1) $
    \i -> numLoop 0 (m-1) $
    \j -> MV.unsafeWrite v (en i j) (f i j)
  return v

-- | /O(rows*cols)/. Identity matrix of the given order.
--
-- > identity n =
-- >                 n
-- >   1 ( 1 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 0 )
-- >     (     ...     )
-- >     ( 0 0 ... 1 0 )
-- >   n ( 0 0 ... 0 1 )
--
identity :: (Num a, V.Unbox a) => Int -> Matrix a
identity n = matrix n n $ \i j -> if i == j then 1 else 0

-- | Similar to 'diagonalList', but using 'V.Vector', which
--   should be more efficient.
diagonal :: (V.Unbox a)
         => a -- ^ Default element
         -> V.Vector a  -- ^ Diagonal vector
         -> Matrix a
diagonal e v = matrix n n $ \i j -> if i == j then V.unsafeIndex v i else e
  where
    n = V.length v

-- | Create a matrix from a non-empty list given the desired size.
--   The list must have at least /rows*cols/ elements.
--   An example:
--
-- >                       ( 1 2 3 )
-- >                       ( 4 5 6 )
-- > fromList 3 3 [1..] =  ( 7 8 9 )
--
fromList :: (V.Unbox a)
         => Int -- ^ Rows
         -> Int -- ^ Columns
         -> [a] -- ^ List of elements
         -> Matrix a
{-# INLINE fromList #-}
fromList n m = M n m 0 . V.fromListN (n*m)

-- | Create a matrix from a non-empty list of non-empty lists.
--   /Each list must have at least as many elements as the first list/.
--   Examples:
--
-- > fromLists [ [1,2,3]      ( 1 2 3 )
-- >           , [4,5,6]      ( 4 5 6 )
-- >           , [7,8,9] ] =  ( 7 8 9 )
--
-- > fromLists [ [1,2,3  ]     ( 1 2 3 )
-- >           , [4,5,6,7]     ( 4 5 6 )
-- >           , [8,9,0  ] ] = ( 8 9 0 )
--
fromLists :: (V.Unbox a)
         => [[a]] -> Matrix a
{-# INLINE fromLists #-}
fromLists [] = error "fromLists: empty list."
fromLists (xs:xss) = fromList n m $ concat $ xs : fmap (take m) xss
  where
    n = 1 + length xss
    m = length xs

-- | Get the elements of a matrix stored in a list of lists,
--   where each list contains the elements of a single row.
--
-- >         ( 1 2 3 )   [ [1,2,3]
-- >         ( 4 5 6 )   , [4,5,6]
-- > toLists ( 7 8 9 ) = , [7,8,9] ]
--
toLists :: (V.Unbox a)
         => Matrix a -> [[a]]
toLists m = [ [ unsafeGet i j m | j <- [0 .. pred $ ncols m] ] | i <- [0 .. pred $ nrows m] ]

-- | /O(1)/. Represent a vector as a one row matrix.
rowVector :: (V.Unbox a)
         => V.Vector a -> Matrix a
rowVector v = M 1 m 0 v
  where
    m = V.length v

-- | /O(1)/. Represent a vector as a one column matrix.
colVector :: (V.Unbox a)
         => V.Vector a -> Matrix a
colVector v = M (V.length v) 1 0 v

-- | /O(rows*cols)/. Permutation matrix.
--
-- > permMatrix n i j =
-- >               i     j       n
-- >   1 ( 1 0 ... 0 ... 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   i ( 0 0 ... 0 ... 1 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   j ( 0 0 ... 1 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >     ( 0 0 ... 0 ... 0 ... 1 0 )
-- >   n ( 0 0 ... 0 ... 0 ... 0 1 )
--
-- When @i == j@ it reduces to 'identity' @n@.
--
permMatrix :: (Num a, V.Unbox a)
           => Int -- ^ Size of the matrix.
           -> Int -- ^ Permuted row 1.
           -> Int -- ^ Permuted row 2.
           -> Matrix a -- ^ Permutation matrix.
permMatrix n r1 r2 | r1 == r2 = identity n
permMatrix n r1 r2 = matrix n n f
 where
  f i j
   | i == r1 = if j == r2 then 1 else 0
   | i == r2 = if j == r1 then 1 else 0
   | i == j = 1
   | otherwise = 0

-------------------------------------------------------
-------------------------------------------------------
---- ACCESSING

-- | /O(1)/. Get an element of a matrix. Indices range from /(1,1)/ to /(n,m)/.
--   It returns an 'error' if the requested element is outside of range.
getElem :: (V.Unbox a)
        => Int      -- ^ Row
        -> Int      -- ^ Column
        -> Matrix a -- ^ Matrix
        -> a
{-# INLINE getElem #-}
getElem i j m =
  fromMaybe
    (error $
       "getElem: Trying to get the "
        ++ show (i, j)
        ++ " element from a matrix."
    )
    (safeGet i j m)

-- | /O(1)/. Unsafe variant of 'getElem', without bounds checking.
unsafeGet :: (V.Unbox a)
          => Int      -- ^ Row
          -> Int      -- ^ Column
          -> Matrix a -- ^ Matrix
          -> a
{-# INLINE unsafeGet #-}
unsafeGet i j (M _ m o v) =
  V.unsafeIndex v $
    if idx < l
      then
        idx
      else
        idx - l
 where
  l = V.length v
  idx = o + encode m i j


-- | Short alias for 'getElem'.
(!) :: (V.Unbox a)
    => Matrix a -> (Int,Int) -> a
{-# INLINE (!) #-}
m ! (i,j) = getElem i j m

-- | Variant of 'getElem' that returns Maybe instead of an error.
safeGet :: (V.Unbox a)
         => Int -> Int -> Matrix a -> Maybe a
safeGet i j a@(M n m _ _)
 | i > n || j > m || i < 1 || j < 1 = Nothing
 | otherwise = Just $ unsafeGet i j a

-- | /O(1)/. Get a row of a matrix as a vector.
getRow :: (V.Unbox a)
       => Int -> Matrix a -> V.Vector a
{-# INLINE getRow #-}
getRow i (M _ m o v)
  | start <= l - m = V.slice start     m         v
  | start  < l     = V.slice start     (l-start) v V.++
                     V.slice 0         (end-l)   v
  | start <= 2*l-m = V.slice (start-l) m         v
  | start  < 2*l   = V.slice (start-l) (2*l-start) v V.++
                     V.slice 0         (end-2*l)   v
  | otherwise = error "out of range"
 where
  l = V.length v
  start = m*i + o
  end = start + m

-- | /O(rows)/. Get a column of a matrix as a vector.
getCol :: (V.Unbox a)
       => Int -> Matrix a -> V.Vector a
{-# INLINE getCol #-}
getCol j m@(M n _ _ _) =
  V.generate n $ \i -> unsafeGet i j m

-------------------------------------------------------
-------------------------------------------------------
---- MATRIX OPERATIONS

-- | Perform an operation element-wise.
--   The second matrix must have at least as many rows
--   and columns as the first matrix. If it's bigger,
--   the leftover items will be ignored.
--   If it's smaller, it will cause a run-time error.
--   You may want to use 'elementwiseUnsafe' if you
--   are definitely sure that a run-time error won't
--   arise.
elementwise :: (V.Unbox a, V.Unbox b, V.Unbox c)
            => (a -> b -> c)
            -> (Matrix a -> Matrix b -> Matrix c)
elementwise f m m' = matrix (nrows m) (ncols m) $
  \i j -> f (getElem i j m) (getElem i j m')

-- | Unsafe version of 'elementwise', but faster.
elementwiseUnsafe :: (V.Unbox a, V.Unbox b, V.Unbox c)
                  => (a -> b -> c) -> (Matrix a -> Matrix b -> Matrix c)
{-# INLINE elementwiseUnsafe #-}
elementwiseUnsafe f m m' = matrix (nrows m) (ncols m) $
  \i j -> f (unsafeGet i j m) (unsafeGet i j m')



-------------------------------------------------------
-------------------------------------------------------
---- TRANSFORMATIONS

-- | Scale a matrix by a given factor.
--   Example:
--
-- >               ( 1 2 3 )   (  2  4  6 )
-- >               ( 4 5 6 )   (  8 10 12 )
-- > scaleMatrix 2 ( 7 8 9 ) = ( 14 16 18 )
scaleMatrix :: (Num a, V.Unbox a)
            => a -> Matrix a -> Matrix a
scaleMatrix = mapMat . (*)
