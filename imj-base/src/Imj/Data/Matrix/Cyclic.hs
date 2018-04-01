-- adapted from https://github.com/Daniel-Diaz/matrix to use an unboxed vector,
-- allow indices rotation, and 0-index the matrix.

{-# LANGUAGE NoImplicitPrelude #-}
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
  -- * Variations
  -- ** Rotations
  , RotationOrder(..)
  , produceRotations, countRotations, countRotations'
  -- * Interleaved
  , produceUsefulInterleavedVariations
  , countUsefulInterleavedVariations2D
    -- ** Special matrices
  , zero
  , identity
  , diagonal
  , permMatrix
    -- * List conversions
  , fromList , fromLists, toLists
    -- * Accessing
  , getElem , (!) , unsafeGet, safeGet, getRow, getCol
  , unsafeGetByIndex
    -- * Matrix operations
  , elementwise, elementwiseUnsafe, mapMat
    -- * Linear transformations
  , scaleMatrix
  -- * Reexports
  , Unbox
  ) where

import           Imj.Prelude
import           Prelude(length, and)

import           Control.DeepSeq(NFData(..))
import           Control.Loop (numLoop)
import           Data.Binary(Binary(..))
import           GHC.Generics (Generic)
-- Data
import           Data.List(foldl')
import           Data.Vector.Unboxed(Unbox)
import qualified Data.Vector.Unboxed         as V hiding(Unbox)
import qualified Data.Vector.Unboxed.Mutable as MV

import           Imj.Geo.Discrete.Types
import           Imj.Geo.Discrete.Interleave(mkInterleaveData, interleaveIdx, countUsefulInterleavedVariations)
import qualified Imj.Data.Matrix.Unboxed as Mat

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
instance (Unbox a, Show a)
        => Show (Matrix a) where
  show x@(M _ _ rot _) = show ("Cyclic rotation " ++ show rot, Mat.fromLists $ toLists x)
instance (Eq a, Unbox a)
        => Eq (Matrix a) where
  m1 == m2 =
    let r = nrows m1
        c = ncols m1
    in  and $ (r == nrows m2) : (c == ncols m2)
            : [ m1 ! (i,j) == m2 ! (i,j) | i <- [0 .. r-1] , j <- [0 .. c-1] ]

instance NFData (Matrix a) where
 rnf = rnf . mvect

instance (Binary a, Unbox a) => Binary (Matrix a) where
  put (M a b c d) = do
    put a
    put b
    put c
    put $ V.toList d
  get = do
    a <- get
    b <- get
    c <- get
    d <- V.fromList <$> get
    return $ M a b c d

data RotationOrder =
    Order0 -- no rotation is produced
  | Order1 -- rotation across all rows, then rotation across all columns
  | Order2 -- rotation across all rows + columns at the same time
  | AtDistance1 -- rotations : [(r,c) | r <- [-1..1], c <- [-1..1], (r,c) /= (0,0)], or [] if the matrix is too small.
  deriving(Show)

countRotations :: (Unbox a)
               => RotationOrder -> Matrix a -> Int
countRotations Order0 _ = 0
countRotations Order1 (M n m _ _) = n + m - 1
countRotations Order2 (M _ _ _ v) = V.length v - 1
countRotations AtDistance1 x =
  if canHaveAtDistance1Rotations x
    then
      8
    else
      0

countRotations' :: RotationOrder -> Size -> Int
countRotations' Order0 _ = 0
countRotations' Order1 (Size (Length x) (Length y)) = x + y - 1
countRotations' Order2 (Size (Length x) (Length y)) = x * y - 1
countRotations' AtDistance1 (Size (Length x) (Length y)) =
  if x < 3 || y < 3
    then
      0
    else
      8

{-# INLINABLE produceRotations #-}
produceRotations :: Unbox a
                 => RotationOrder -> Matrix a -> [Matrix a]
produceRotations ro x@(M r c _ v) =
  -- note that we could take the matrix rotation into account,
  -- but in practice we use this function with 0 rotation matrices only so it doesn't make a difference.
  map (setRotation x) $ case ro of
    Order0 -> []
    Order1 -> [1.. c-1] ++ map (*c) [1..r-1]
    Order2 -> [1..countRotations Order2 x]
    AtDistance1 ->
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
canHaveAtDistance1Rotations (M r c _ _) = not $ c < 3 || r < 3 -- to avoid duplicate rotations

setRotation :: (Unbox a) => Matrix a -> Int -> Matrix a
setRotation m@(M _ _ _ v) i
  | i < 0 = error "negative offset"
  | i >= V.length v = error "out of range offset"
  | otherwise = unsafeSetRotation m i

{-# INLINE unsafeSetRotation #-}
unsafeSetRotation :: Matrix a -> Int -> Matrix a
unsafeSetRotation m i = m { rotation = i }

mapMat :: (Unbox a, Unbox b)
        => (a -> b)
        -> Matrix a -> Matrix b
mapMat func (M a b c v) = M a b c $ V.map func v


-- TODO should we use foldr or foldl'?
produceUsefulInterleavedVariations :: Unbox a
                                    => Matrix a
                                    -> [Matrix a]
produceUsefulInterleavedVariations x =
  snd $ foldl'
    (\(m,prevResults) i ->
      let fi = bool (reorderRows interleaveRows) id $ i == 0
          m' = fi m
          (_,intermediateResults) = foldl'
            (\(n, l) j ->
              let fj = bool (reorderCols interleaveCols) id $ j == 0
                  n' = fj n
              in (n', n':l))
            (m', prevResults)
            [0..nColVar-1]
      in (m', intermediateResults))
    (x,[])
    [0..nRowVar-1]
 where
  (nRowVar, interleaveRows) = getInterleavedInfos $ nrows x
  (nColVar, interleaveCols) = getInterleavedInfos $ ncols x

-- Counts the number of matrices returned by 'produceUsefulInterleavedVariations'
countUsefulInterleavedVariations2D :: Size -> Int
countUsefulInterleavedVariations2D (Size (Length x) (Length y)) =
  countUsefulInterleavedVariations x * countUsefulInterleavedVariations y

getInterleavedInfos :: Int
                    -- ^ the length of the array that will be interleaved
                    -> (Int, Int -> Int)
                    -- ^ fst: The count of useful variations
                    -- , snd : the function to get interleaved indices
getInterleavedInfos d =
  let !iD = mkInterleaveData d
  in (countUsefulInterleavedVariations d, interleaveIdx iD)

reorderRows, reorderCols :: (Unbox a)
                         => (Int -> Int)
                         -> Matrix a
                         -> Matrix a
reorderRows order m =
  matrix (nrows m) (ncols m) $ \i j ->
    unsafeGet (order i) j m
reorderCols order m =
  matrix (nrows m) (ncols m) $ \i j ->
    unsafeGet i (order j) m

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
zero :: (Num a, Unbox a) =>
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
matrix :: (Unbox a)
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
identity :: (Num a, Unbox a) => Int -> Matrix a
identity n = matrix n n $ \i j -> if i == j then 1 else 0

-- | Similar to 'diagonalList', but using 'V.Vector', which
--   should be more efficient.
diagonal :: (Unbox a)
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
fromList :: (Unbox a)
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
fromLists :: (Unbox a)
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
toLists :: (Unbox a)
         => Matrix a -> [[a]]
toLists m@(M r c _ _) = [ [ unsafeGet i j m | j <- [0 .. c - 1] ] | i <- [0 .. r - 1] ] -- TODO optimize by slicing

-- | /O(1)/. Represent a vector as a one row matrix.
rowVector :: (Unbox a)
         => V.Vector a -> Matrix a
rowVector v = M 1 m 0 v
  where
    m = V.length v

-- | /O(1)/. Represent a vector as a one column matrix.
colVector :: (Unbox a)
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
permMatrix :: (Num a, Unbox a)
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
getElem :: (Unbox a)
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
unsafeGet :: (Unbox a)
          => Int      -- ^ Row
          -> Int      -- ^ Column
          -> Matrix a -- ^ Matrix
          -> a
{-# INLINE unsafeGet #-}
unsafeGet i j mat@(M _ m _ _) = unsafeGetByIndex (encode m i j) mat


unsafeGetByIndex :: (Unbox a)
                 => Int      -- ^ Index
                 -> Matrix a -- ^ Matrix
                 -> a
{-# INLINE unsafeGetByIndex #-}
unsafeGetByIndex idx (M _ _ o v) =
  V.unsafeIndex v $ bool (rawIdx - l) rawIdx $ rawIdx < l
 where
  l = V.length v
  rawIdx = idx + o

-- | Short alias for 'getElem'.
(!) :: (Unbox a)
    => Matrix a -> (Int,Int) -> a
{-# INLINE (!) #-}
m ! (i,j) = getElem i j m

-- | Variant of 'getElem' that returns Maybe instead of an error.
safeGet :: (Unbox a)
         => Int -> Int -> Matrix a -> Maybe a
safeGet i j a@(M n m _ _)
 | i >= n || j >= m || i < 0 || j < 0 = Nothing
 | otherwise = Just $ unsafeGet i j a

-- | /O(1)/. Get a row of a matrix as a vector.
getRow :: (Unbox a)
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
getCol :: (Unbox a)
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
elementwise :: (Unbox a, Unbox b, Unbox c)
            => (a -> b -> c)
            -> (Matrix a -> Matrix b -> Matrix c)
elementwise f m m' = matrix (nrows m) (ncols m) $
  \i j -> f (getElem i j m) (getElem i j m')

-- | Unsafe version of 'elementwise', but faster.
elementwiseUnsafe :: (Unbox a, Unbox b, Unbox c)
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
scaleMatrix :: (Num a, Unbox a)
            => a -> Matrix a -> Matrix a
scaleMatrix = mapMat . (*)
