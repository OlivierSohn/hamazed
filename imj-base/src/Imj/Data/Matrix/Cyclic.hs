-- adapted from https://github.com/Daniel-Diaz/matrix to use an unboxed vector,
-- allow indices rotation, and 0-index the matrix.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Imj.Data.Matrix.Cyclic (
    -- * Matrix type
    Matrix
  , nelems, nrows, ncols, rotation, mvect
    -- * Builders
  , matrix
  , rowVector
  , colVector
  , setRotation
  , unsafeSetRotation
  -- * Variations
  -- ** Rotations
  , RotationOrder(..)
  , produceRotations, countRotations, countRotations'
  -- ** Interleaved
  , produceUsefulInterleavedVariations
  -- ** Moduloed
  , modulate
    -- ** Special matrices
  , zero
  , identity
  , diagonal
  , permMatrix
    -- * List conversions
  , fromList, fromLists, fromVector, toLists
    -- * Accessing
  , getElem , unsafeGet, safeGet, getRow, getCol
  , unsafeGetByIndex
    -- * Matrix operations
  , elementwise, elementwiseUnsafe, mapMat
    -- * Linear transformations
  , scaleMatrix
  -- * Reexports
  , Storable(..)
  ) where

import           Imj.Prelude
import           Prelude(length, and)

import           Foreign.Storable(Storable(..))
import           Control.Loop (numLoop)
import           Data.List(foldl', take, concat)
import           Data.Vector.Binary()
import qualified Data.Vector.Storable         as V hiding(Storable)
import qualified Data.Vector.Storable.Mutable as MV

import           Imj.Geo.Discrete.Types
import           Imj.Geo.Discrete.Interleave(InterleaveInfo(..))

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
} deriving (Generic)
instance (Storable a, Show a)
        => Show (Matrix a) where
  show (M _ _ rot _) = show ("Cyclic rotation " ++ show rot)
instance (Eq a, Storable a)
        => Eq (Matrix a) where
  m1 == m2 =
    and $ (nrows m1 == nrows m2) : (ncols m1 == ncols m2) : (rotation m1 == rotation m2) :
        [ unsafeGetByIndex i m1 == unsafeGetByIndex i m2 | i <- [0 .. nelems m1 - 1]]

instance NFData (Matrix a) where
 rnf = rnf . mvect

instance (Binary a, Storable a) => Binary (Matrix a)

data RotationOrder =
    Rect1 -- rotations : [(r,c) | r <- [-1..1], c <- [-1..1], (r,c) /= (0,0)], or [] if the matrix is too small.
  | Order1 -- rotation across all rows, then rotation across all columns
  | Order2 -- rotation across all rows + columns at the same time
  deriving(Generic, Show, Eq, Ord, Lift)
instance Binary RotationOrder
instance NFData RotationOrder

countRotations :: (Storable a)
               => RotationOrder -> Matrix a -> Int
countRotations Order1 (M n m _ _) = n + m - 1
countRotations Order2 (M _ _ _ v) = V.length v - 1
countRotations Rect1 x =
  if canHaveAtDistance1Rotations x
    then
      8
    else
      0

countRotations' :: RotationOrder -> Size -> Int
countRotations' Order1 (Size (Length y) (Length x)) = x + y - 1
countRotations' Order2 (Size (Length y) (Length x)) = x * y - 1
countRotations' Rect1 (Size (Length y) (Length x)) =
  if x < 3 || y < 3
    then
      0
    else
      8

{-# INLINE produceRotations #-}
produceRotations :: Storable a
                 => RotationOrder -> Matrix a -> [Matrix a]
produceRotations ro x@(M r c _ v) =
  -- note that we could take the matrix rotation into account,
  -- but in practice we use this function with 0 rotation matrices only so it doesn't make a difference.
  map (unsafeSetRotation x) $ case ro of
    Order1 -> [1.. c-1] ++ map (*c) [1..r-1]
    Order2 -> [1..countRotations Order2 x]
    Rect1 ->
      if canHaveAtDistance1Rotations x
        then
          concatMap
            (\pos -> [pos, len - pos]) -- include negative rotations
            positiveRotations
        else
          []
     where
      positiveRotations = 1 : map (c +) [-1..1] -- positiveRotations are guaranteed to be < len
      len = V.length v

{-# INLINE canHaveAtDistance1Rotations #-}
canHaveAtDistance1Rotations :: Matrix a -> Bool
canHaveAtDistance1Rotations (M r c _ _) = not $ c < 3 || r < 3 -- to avoid duplicate rotations

setRotation :: (Storable a) => Matrix a -> Int -> Matrix a
setRotation m@(M _ _ _ v) i
  | i < 0 = error "negative offset"
  | i >= V.length v = error "out of range offset"
  | otherwise = unsafeSetRotation m i

{-# INLINE unsafeSetRotation #-}
unsafeSetRotation :: Matrix a -> Int -> Matrix a
unsafeSetRotation m i = m { rotation = i }

modulate :: (Storable a) => Int -> Matrix a -> Matrix a
modulate n mat@(M _ _ _ v)
 | n <= 1 = mat
 | n >= len = error $ "out of range modulo:" ++ show n
 | otherwise =
     mat{ mvect = V.create (do
      mv <- MV.unsafeNew len
      numLoop 0 (nIterationsWithOneMore-1) $ \startIdx ->
        numLoop 0 standardIterationsLength $ \i -> do
          let source = startIdx + i*n
              target = startIdx*(standardIterationsLength+1) + i
          MV.unsafeWrite mv target $ V.unsafeIndex v source
      let start2 = nIterationsWithOneMore * (standardIterationsLength+1)
      numLoop nIterationsWithOneMore (n-1) (\startIdx ->
        numLoop 0 (standardIterationsLength-1) (\i -> do
          let source = startIdx + i*n
              target = start2 + (startIdx-nIterationsWithOneMore)*standardIterationsLength + i
          MV.unsafeWrite mv target $ V.unsafeIndex v source))
      return mv)
    }
  where
    -- when starting at [0..nIterationsWithOneMore-1], there are standardIterationsLength+1 elements.
    -- when starting at [nIterationsWithOneMore..n-1], there are standardIterationsLength elements.
    (standardIterationsLength,nIterationsWithOneMore) = quotRem len n
    len = V.length v

{-# INLINE nelems #-}
nelems :: (Storable a) => Matrix a -> Int
nelems (M _ _ _ v) = V.length v

mapMat :: (Storable a, Storable b)
        => (a -> b)
        -> Matrix a -> Matrix b
mapMat func (M a b c v) = M a b c $ V.map func v

data InterleavedVariationState a = IVS {
    _refMat :: !(Matrix a)
  , _producedMats :: [Matrix a]
}

-- Benchmarks show that foldr is slower than foldl' here.
-- | Note that rotation is applied /after/ the interleaving.
{-# INLINE produceUsefulInterleavedVariations #-}
produceUsefulInterleavedVariations :: Storable a
                                   => InterleaveInfo
                                   -- ^ for rows
                                   -> InterleaveInfo
                                   -- ^ for columns
                                   -> Matrix a
                                   -> [Matrix a]
produceUsefulInterleavedVariations
  (InterleaveInfo nRowVar interleaveRows)
  (InterleaveInfo nColVar interleaveCols)
  x@(M rows cols _ _) =
  -- reorderRows is faster than reorderCols, hence it is done in the inner loop.
  _producedMats $ foldl'
    (\(IVS m prevResults) j ->
      let fj
           | j == 0 = id
           | otherwise = reorderCols
          m' = m{mvect = fj $ mvect m}
          (IVS _ intermediateResults) = foldl'
            (\(IVS n l) i ->
              let fi
                   | i == 0 = id
                   | otherwise = reorderRows
                  n' = n{mvect = fi $ mvect n}
              in IVS n' $ n':l)
            (IVS m' prevResults)
            [0..nRowVar-1]
      in IVS m' intermediateResults)
    (IVS x [])
    [0..nColVar-1]
 where
  len = rows * cols

  reorderRows v = -- ignores rotations
    V.create $ do
      mv <- MV.unsafeNew len
      numLoop 0 (rows-1) $ \i -> do
        let source = V.unsafeSlice (cols*V.unsafeIndex interleaveRows i) cols v
            dest  = MV.unsafeSlice (cols*i)                              cols mv
        V.copy dest source
      return mv

  reorderCols v = -- ignores rotations
    V.create $ do
      mv <- MV.unsafeNew len
      numLoop 0 (rows-1) $ \i -> do
        let rowStart = cols*i
        numLoop 0 (cols-1) $ \j ->
          MV.unsafeWrite mv (rowStart+j) $ V.unsafeIndex v $ rowStart + V.unsafeIndex interleaveCols j
      return mv

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
zero :: (Num a, Storable a) =>
     Int -- ^ Rows
  -> Int -- ^ Columns
  -> Matrix a
{-# INLINE zero #-}
zero n m = M n m 0 $ V.replicate (n*m) 0

-- | /O(rows*cols)/. Generate a 0-rotation matrix from a generator function.
--   Example of usage:
--
-- >                                  (  1  0 -1 -2 )
-- >                                  (  3  2  1  0 )
-- >                                  (  5  4  3  2 )
-- > matrix 4 4 $ \(i,j) -> 2*i - j = (  7  6  5  4 )
matrix :: (Storable a)
       => Int -- ^ Rows
       -> Int -- ^ Columns
       -> (Int -> Int -> Int -> a) -- ^ Generator function
       -> Matrix a
{-# INLINE matrix #-}
matrix n m f = M n m 0 $
  V.create $ do
    v <- MV.unsafeNew $ n * m
    numLoop 0 (n-1) $ \i -> do
      let rowStart = m*i
      numLoop 0 (m-1) $ \j ->
        MV.unsafeWrite v (rowStart+j) (f rowStart i j)
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
identity :: (Num a, Storable a) => Int -> Matrix a
identity n = matrix n n $ \_ i j -> if i == j then 1 else 0

-- | Similar to 'diagonalList', but using 'V.Vector', which
--   should be more efficient.
diagonal :: (Storable a)
         => a -- ^ Default element
         -> V.Vector a  -- ^ Diagonal vector
         -> Matrix a
diagonal e v = matrix n n $ \_ i j -> if i == j then V.unsafeIndex v i else e
  where
    n = V.length v

fromVector :: (Storable a)
           => Int -- ^ Rows
           -> Int -- ^ Columns
           -> V.Vector a -- ^ Vector of elements
           -> Matrix a
{-# INLINE fromVector #-}
fromVector n m v
  | n * m == len = M n m 0 v
  | otherwise = error $ "Wrong size " ++ show (n,m,len)
  where len = V.length v

-- | Create a matrix from a non-empty list given the desired size.
--   The list must have at least /rows*cols/ elements.
--   An example:
--
-- >                       ( 1 2 3 )
-- >                       ( 4 5 6 )
-- > fromList 3 3 [1..] =  ( 7 8 9 )
--
fromList :: (Storable a)
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
fromLists :: (Storable a)
         => [[a]] -> Matrix a
{-# INLINE fromLists #-}
fromLists [] = fromList 0 0 []
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
toLists :: (Storable a)
         => Matrix a -> [[a]]
toLists m@(M r c _ _) = [ [ unsafeGet i j m | j <- [0 .. c - 1] ] | i <- [0 .. r - 1] ] -- TODO optimize by slicing

-- | /O(1)/. Represent a vector as a one row matrix.
rowVector :: (Storable a)
         => V.Vector a -> Matrix a
rowVector v = M 1 m 0 v
  where
    m = V.length v

-- | /O(1)/. Represent a vector as a one column matrix.
colVector :: (Storable a)
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
permMatrix :: (Num a, Storable a)
           => Int -- ^ Size of the matrix.
           -> Int -- ^ Permuted row 1.
           -> Int -- ^ Permuted row 2.
           -> Matrix a -- ^ Permutation matrix.
permMatrix n r1 r2 | r1 == r2 = identity n
permMatrix n r1 r2 = matrix n n f
 where
  f _ i j
   | i == r1 = if j == r2 then 1 else 0
   | i == r2 = if j == r1 then 1 else 0
   | i == j = 1
   | otherwise = 0

-------------------------------------------------------
-------------------------------------------------------
---- ACCESSING

-- | /O(1)/. Get an element of a matrix. Indices range from /(1,1)/ to /(n,m)/.
--   It returns an 'error' if the requested element is outside of range.
getElem :: (Storable a)
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
unsafeGet :: (Storable a)
          => Int      -- ^ Row
          -> Int      -- ^ Column
          -> Matrix a -- ^ Matrix
          -> a
{-# INLINE unsafeGet #-}
unsafeGet i j mat@(M _ m _ _) = unsafeGetByIndex (encode m i j) mat


unsafeGetByIndex :: (Storable a)
                 => Int      -- ^ Index
                 -> Matrix a -- ^ Matrix
                 -> a
{-# INLINE unsafeGetByIndex #-}
unsafeGetByIndex idx (M _ _ o v) =
  V.unsafeIndex v $ bool (rawIdx - l) rawIdx $ rawIdx < l
 where
  l = V.length v
  rawIdx = idx + o


-- | Variant of 'getElem' that returns Maybe instead of an error.
safeGet :: (Storable a)
         => Int -> Int -> Matrix a -> Maybe a
safeGet i j a@(M n m _ _)
 | i >= n || j >= m || i < 0 || j < 0 = Nothing
 | otherwise = Just $ unsafeGet i j a

-- | /O(1)/. Get a row of a matrix as a vector.
getRow :: (Storable a)
       => Int -> Matrix a -> V.Vector a
{-# INLINE getRow #-}
getRow i (M _ m o v)
  | start <= l - m = V.unsafeSlice start     m         v
  | start  < l     = V.unsafeSlice start     (l-start) v V.++
                     V.unsafeSlice 0         (end-l)   v
  | start <= 2*l-m = V.unsafeSlice (start-l) m         v
  | start  < 2*l   = V.unsafeSlice (start-l) (2*l-start) v V.++
                     V.unsafeSlice 0         (end-2*l)   v
  | otherwise = error "out of range"
 where
  l = V.length v
  start = m*i + o
  end = start + m

-- | /O(rows)/. Get a column of a matrix as a vector.
getCol :: (Storable a)
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
elementwise :: (Storable a, Storable b, Storable c)
            => (a -> b -> c)
            -> (Matrix a -> Matrix b -> Matrix c)
elementwise f m m' = matrix (nrows m) (ncols m) $
  \_ i j -> f (getElem i j m) (getElem i j m')

-- | Unsafe version of 'elementwise', but faster.
elementwiseUnsafe :: (Storable a, Storable b, Storable c)
                  => (a -> b -> c) -> (Matrix a -> Matrix b -> Matrix c)
{-# INLINE elementwiseUnsafe #-}
elementwiseUnsafe f m m' = matrix (nrows m) (ncols m) $
  \_ i j -> f (unsafeGet i j m) (unsafeGet i j m')



-------------------------------------------------------
-------------------------------------------------------
---- TRANSFORMATIONS

-- | Scale a matrix by a given factor.
--   Example:
--
-- >               ( 1 2 3 )   (  2  4  6 )
-- >               ( 4 5 6 )   (  8 10 12 )
-- > scaleMatrix 2 ( 7 8 9 ) = ( 14 16 18 )
scaleMatrix :: (Num a, Storable a)
            => a -> Matrix a -> Matrix a
scaleMatrix = mapMat . (*)
