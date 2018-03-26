-- adapted from https://github.com/Daniel-Diaz/matrix to use an unboxed vector
-- and 0-index the matrix.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Data.Matrix.Unboxed (
    -- * Matrix type
    Matrix , prettyMatrix
  , nrows , ncols
    -- * Builders
  , matrix
  , rowVector
  , colVector
    -- ** Special matrices
  , zero
  , identity
  , diagonalList
  , diagonal
    -- * List conversions
  , fromList , fromLists
  , toList   , toLists
    -- * Accessing
  , getElem , (!) , unsafeGet , safeGet, safeSet
  , getRow  , safeGetRow , getCol , safeGetCol
  , getDiag
    -- * Manipulating matrices
  , setElem
  , unsafeSet
  , transpose , setSize , extendTo
  , mapRow , mapCol, mapPos
    -- * Matrix operations
  , elementwise, elementwiseUnsafe
    -- * Matrix multiplication
    -- ** About matrix multiplication
    -- $mult

    -- ** Functions
  , multStd
    -- * Linear transformations
  , scaleMatrix
  , scaleRow
  , combineRows
  , switchRows
  , switchCols
  -- * Reexports
  , Unbox
  ) where

import           Imj.Prelude
import           Prelude(length, and, unlines)

import           Control.DeepSeq
import           Control.Loop (numLoop, numLoopFold)
import           Data.Binary(Binary(..))
import qualified Data.Semigroup as S
import           GHC.Generics (Generic)
-- Data
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Vector.Unboxed(Unbox)
import qualified Data.Vector                     as BV
import qualified Data.Vector.Unboxed             as V hiding(Unbox)
import qualified Data.Vector.Unboxed.Mutable     as MV
import qualified Data.Matrix                     as Mat -- this matrix is 1-indexed, whereas ours is 0-indexed

-------------------------------------------------------
-------------------------------------------------------
---- MATRIX TYPE

encode :: Int -> (Int,Int) -> Int
{-# INLINE encode #-}
encode m (i,j) = i*m + j

decode :: Int -> Int -> (Int,Int)
{-# INLINE decode #-}
decode m k = (q,r)
 where
  (q,r) = quotRem k m

-- | Type of matrices.
--
--   Elements can be of any type. Rows and columns
--   are 0-indexed
data Matrix a = M {
   nrows     :: {-# UNPACK #-} !Int -- ^ Number of rows.
 , ncols     :: {-# UNPACK #-} !Int -- ^ Number of columns.
 , mvect     :: !(V.Vector a)          -- ^ Content of the matrix as a plain vector.
   } deriving (Generic)

instance (Eq a, Unbox a)
        => Eq (Matrix a) where
  m1 == m2 =
    let r = nrows m1
        c = ncols m1
    in  and $ (r == nrows m2) : (c == ncols m2)
            : [ m1 ! (i,j) == m2 ! (i,j) | i <- [0 .. r-1] , j <- [0 .. c-1] ]

-- | Just a cool way to output the size of a matrix.
sizeStr :: Int -> Int -> String
sizeStr n m = show n ++ "x" ++ show m

-- | Display a matrix as a 'String' using the 'Show' instance of its elements.
prettyMatrix :: (Show a, Unbox a) => Matrix a -> String
prettyMatrix m = concat
   [ "┌ ", unwords (replicate (ncols m) blank), " ┐\n"
   , unlines
   [ "│ " ++ unwords (fmap (\j -> fill $ strings Mat.! (i,j)) [1..ncols m]) ++ " │" | i <- [1.. nrows m] ]
   , "└ ", unwords (replicate (ncols m) blank), " ┘"
   ]
 where
   strings = mapMat' show m
   widest = maximum $ map length $ Mat.toList strings
   fill str = replicate (widest - length str) ' ' ++ str
   blank = fill ""


instance (Show a, Unbox a) => Show (Matrix a) where
 show = prettyMatrix


instance NFData (Matrix a) where
 rnf = rnf . mvect

instance (Binary a, Unbox a) => Binary (Matrix a) where
  put (M a b c) = do
    put a
    put b
    put $ V.toList c
  get = do
    a <- get
    b <- get
    c <- V.fromList <$> get
    return $ M a b c

-------------------------------------------------------
-------------------------------------------------------

-------------------------------------------------------
-------------------------------------------------------
---- MONOID INSTANCE

instance (Monoid a, Unbox a) => S.Semigroup (Matrix a) where
  (<>) = mappend

instance (Monoid a, Unbox a) => Monoid (Matrix a) where
  mempty = fromList 1 1 [mempty]
  mappend m m' = matrix (max (nrows m) (nrows m')) (max (ncols m) (ncols m')) $ uncurry zipTogether
    where zipTogether row column = fromMaybe mempty $ safeGet row column m <> safeGet row column m'


mapMat :: (Unbox a, Unbox b)
        => (a -> b)
        -> Matrix a -> Matrix b
mapMat func (M a b v) = M a b $ V.map func v

mapMat' :: (Unbox a)
        => (a -> b)
        -> Matrix a -> Mat.Matrix b
mapMat' func m = Mat.matrix (nrows m) (ncols m) (\(i,j) -> func $ unsafeGet (i-1) (j-1) m)

-- | /O(rows*cols)/. Map a function over a row.
--   Example:
--
-- >                          ( 1 2 3 )   ( 1 2 3 )
-- >                          ( 4 5 6 )   ( 5 6 7 )
-- > mapRow (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 8 9 )
--
mapRow :: (Unbox a)
        => (Int -> a -> a) -- ^ Function takes the current column as additional argument.
        -> Int            -- ^ Row to map.
        -> Matrix a -> Matrix a
mapRow f r m =
  matrix (nrows m) (ncols m) $ \(i,j) ->
    let a = unsafeGet i j m
    in  if i == r
           then f j a
           else a

-- | /O(rows*cols)/. Map a function over a column.
--   Example:
--
-- >                          ( 1 2 3 )   ( 1 3 3 )
-- >                          ( 4 5 6 )   ( 4 6 6 )
-- > mapCol (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 9 9 )
--
mapCol :: (Unbox a)
        => (Int -> a -> a) -- ^ Function takes the current row as additional argument.
        -> Int            -- ^ Column to map.
        -> Matrix a -> Matrix a
mapCol f c m =
  matrix (nrows m) (ncols m) $ \(i,j) ->
    let a = unsafeGet i j m
    in  if j == c
           then f i a
           else a


-- | /O(rows*cols)/. Map a function over elements.
--   Example:
--
-- >                            ( 1 2 3 )   ( 0 -1 -2 )
-- >                            ( 4 5 6 )   ( 1  0 -1 )
-- > mapPos (\(r,c) a -> r - c) ( 7 8 9 ) = ( 2  1  0 )
--
mapPos :: (Unbox a, Unbox b)
        => ((Int, Int) -> a -> b) -- ^ Function takes the current Position as additional argument.
        -> Matrix a
        -> Matrix b
mapPos f m@M {ncols = cols, mvect = vect}=
  m { mvect = V.imap (\i e -> f (decode cols i) e) vect}

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
zero n m = M n m $ V.replicate (n*m) 0

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
       -> ((Int,Int) -> a) -- ^ Generator function
       -> Matrix a
{-# INLINE matrix #-}
matrix n m f = M n m $ V.create $ do
  v <- MV.new $ n * m
  let en = encode m
  numLoop 0 (n-1) $
    \i -> numLoop 0 (m-1) $
    \j -> MV.unsafeWrite v (en (i,j)) (f (i,j))
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
identity n = matrix n n $ \(i,j) -> if i == j then 1 else 0

-- | Similar to 'diagonalList', but using 'V.Vector', which
--   should be more efficient.
diagonal :: (Unbox a)
         => a -- ^ Default element
         -> V.Vector a  -- ^ Diagonal vector
         -> Matrix a
diagonal e v = matrix n n $ \(i,j) -> if i == j then V.unsafeIndex v i else e
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
fromList n m = M n m . V.fromListN (n*m)

-- | Get the elements of a matrix stored in a list.
--
-- >        ( 1 2 3 )
-- >        ( 4 5 6 )
-- > toList ( 7 8 9 ) = [1,2,3,4,5,6,7,8,9]
--
toList :: (Unbox a)
         => Matrix a -> [a]
toList m = [ unsafeGet i j m | i <- [0 .. pred $ nrows m] , j <- [0 .. pred $ ncols m] ]

-- | Get the elements of a matrix stored in a list of lists,
--   where each list contains the elements of a single row.
--
-- >         ( 1 2 3 )   [ [1,2,3]
-- >         ( 4 5 6 )   , [4,5,6]
-- > toLists ( 7 8 9 ) = , [7,8,9] ]
--
toLists :: (Unbox a)
         => Matrix a -> [[a]]
toLists m = [ [ unsafeGet i j m | j <- [0 .. pred $ ncols m] ] | i <- [0 .. pred $ nrows m] ]

-- | Diagonal matrix from a non-empty list given the desired size.
--   Non-diagonal elements will be filled with the given default element.
--   The list must have at least /order/ elements.
--
-- > diagonalList n 0 [1..] =
-- >                   n
-- >   1 ( 1 0 ... 0   0 )
-- >   2 ( 0 2 ... 0   0 )
-- >     (     ...       )
-- >     ( 0 0 ... n-1 0 )
-- >   n ( 0 0 ... 0   n )
--
diagonalList :: (Unbox a)
         => Int -> a -> [a] -> Matrix a
diagonalList n e xs = matrix n n $ \(i,j) -> if i == j then xs !! i else e

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

-- | /O(1)/. Represent a vector as a one row matrix.
rowVector :: (Unbox a)
         => V.Vector a -> Matrix a
rowVector v = M 1 m v
  where
    m = V.length v

-- | /O(1)/. Represent a vector as a one column matrix.
colVector :: (Unbox a)
         => V.Vector a -> Matrix a
colVector v = M (V.length v) 1 v


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
        ++ " element from a "
        ++ sizeStr (nrows m) (ncols m)
        ++ " matrix."
    )
    (safeGet i j m)

-- | /O(1)/. Unsafe variant of 'getElem', without bounds checking.
unsafeGet :: (Unbox a)
          => Int      -- ^ Row
          -> Int      -- ^ Column
          -> Matrix a -- ^ Matrix
          -> a
{-# INLINE unsafeGet #-}
unsafeGet i j (M _ m v) = V.unsafeIndex v $ encode m (i,j)

-- | Short alias for 'getElem'.
(!) :: (Unbox a)
    => Matrix a -> (Int,Int) -> a
{-# INLINE (!) #-}
m ! (i,j) = getElem i j m

-- | Internal alias for 'unsafeGet'.
(!.) :: (Unbox a)
     => Matrix a -> (Int,Int) -> a
{-# INLINE (!.) #-}
m !. (i,j) = unsafeGet i j m

-- | Variant of 'getElem' that returns Maybe instead of an error.
safeGet :: (Unbox a)
         => Int -> Int -> Matrix a -> Maybe a
safeGet i j a@(M n m _)
 | i >= n || j >= m || i < 0 || j < 0 = Nothing
 | otherwise = Just $ unsafeGet i j a

-- | Variant of 'setElem' that returns Maybe instead of an error.
safeSet :: (Unbox a)
        => a -> (Int, Int) -> Matrix a -> Maybe (Matrix a)
safeSet x p@(i,j) a@(M n m _)
 | i >= n || j >= m || i < 0 || j < 0 = Nothing
 | otherwise = Just $ unsafeSet x p a

-- | /O(1)/. Get a row of a matrix as a vector.
getRow :: (Unbox a)
       => Int -> Matrix a -> V.Vector a
{-# INLINE getRow #-}
getRow i (M _ m v) = V.slice (m*i) m v

-- | Varian of 'getRow' that returns a maybe instead of an error
safeGetRow :: (Unbox a)
           => Int -> Matrix a -> Maybe (V.Vector a)
safeGetRow r m
    | r >= nrows m || r < 0 = Nothing
    | otherwise = Just $ getRow r m

-- | /O(rows)/. Get a column of a matrix as a vector.
getCol :: (Unbox a)
       => Int -> Matrix a -> V.Vector a
{-# INLINE getCol #-}
getCol j (M n m v) = V.generate n $ \i -> v V.! encode m (i,j)

-- | Varian of 'getColumn' that returns a maybe instead of an error
safeGetCol :: (Unbox a)
           => Int -> Matrix a -> Maybe (V.Vector a)
safeGetCol c m
    | c >= ncols m || c < 0 = Nothing
    | otherwise = Just $ getCol c m

-- | /O(min rows cols)/. Diagonal of a /not necessarily square/ matrix.
getDiag :: (Unbox a)
        => Matrix a -> V.Vector a
getDiag m = V.generate k $ \i -> m ! (i,i)
 where
  k = min (nrows m) (ncols m)

-------------------------------------------------------
-------------------------------------------------------
---- MANIPULATING MATRICES

msetElem :: (Unbox a, PrimMonad m)
         => a -- ^ New element
         -> Int -- ^ Number of columns of the matrix
         -> (Int,Int) -- ^ Position to set the new element
         -> MV.MVector (PrimState m) a -- ^ Mutable vector
         -> m ()
{-# INLINE msetElem #-}
msetElem x w (i,j) v = MV.write v (encode w (i,j)) x

unsafeMset :: (Unbox a, PrimMonad m)
         => a -- ^ New element
         -> Int -- ^ Number of columns of the matrix
         -> (Int,Int) -- ^ Position to set the new element
         -> MV.MVector (PrimState m) a -- ^ Mutable vector
         -> m ()
{-# INLINE unsafeMset #-}
unsafeMset x w (i,j) v = MV.unsafeWrite v (encode w (i,j)) x

-- | Replace the value of a cell in a matrix.
setElem :: Unbox a
        => a -- ^ New value.
        -> (Int,Int) -- ^ Position to replace.
        -> Matrix a -- ^ Original matrix.
        -> Matrix a -- ^ Matrix with the given position replaced with the given value.
{-# INLINE setElem #-}
setElem x p (M n m v) = M n m $ V.modify (msetElem x m p) v

-- | Unsafe variant of 'setElem', without bounds checking.
unsafeSet :: Unbox a
        => a -- ^ New value.
        -> (Int,Int) -- ^ Position to replace.
        -> Matrix a -- ^ Original matrix.
        -> Matrix a -- ^ Matrix with the given position replaced with the given value.
{-# INLINE unsafeSet #-}
unsafeSet x p (M n m v) = M n m $ V.modify (unsafeMset x m p) v

-- | /O(rows*cols)/. The transpose of a matrix.
--   Example:
--
-- >           ( 1 2 3 )   ( 1 4 7 )
-- >           ( 4 5 6 )   ( 2 5 8 )
-- > transpose ( 7 8 9 ) = ( 3 6 9 )
transpose :: Unbox a
          => Matrix a -> Matrix a
transpose m = matrix (ncols m) (nrows m) $ \(i,j) -> m ! (j,i)

-- | Extend a matrix to a given size adding a default element.
--   If the matrix already has the required size, nothing happens.
--   The matrix is /never/ reduced in size.
--   Example:
--
-- >                            ( 1 2 3 0 0 )
-- >                ( 1 2 3 )   ( 4 5 6 0 0 )
-- >                ( 4 5 6 )   ( 7 8 9 0 0 )
-- > extendTo 0 4 5 ( 7 8 9 ) = ( 0 0 0 0 0 )
--
-- The definition of 'extendTo' is based on 'setSize':
--
-- > extendTo e n m a = setSize e (max n $ nrows a) (max m $ ncols a) a
--
extendTo :: (Unbox a)
         => a   -- ^ Element to add when extending.
         -> Int -- ^ Minimal number of rows.
         -> Int -- ^ Minimal number of columns.
         -> Matrix a -> Matrix a
extendTo e n m a = setSize e (max n $ nrows a) (max m $ ncols a) a

-- | Set the size of a matrix to given parameters. Use a default element
--   for undefined entries if the matrix has been extended.
setSize :: (Unbox a)
        => a   -- ^ Default element.
        -> Int -- ^ Number of rows.
        -> Int -- ^ Number of columns.
        -> Matrix a
        -> Matrix a
{-# INLINE setSize #-}
setSize e n m a@(M n0 m0 _) = matrix n m $ \(i,j) ->
  if i < n0 && j < m0
     then unsafeGet i j a
     else e

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
            => (a -> b -> c) -> (Matrix a -> Matrix b -> Matrix c)
elementwise f m m' = matrix (nrows m) (ncols m) $
  \k -> f (m ! k) (m' ! k)

-- | Unsafe version of 'elementwise', but faster.
elementwiseUnsafe :: (Unbox a, Unbox b, Unbox c)
                  => (a -> b -> c) -> (Matrix a -> Matrix b -> Matrix c)
{-# INLINE elementwiseUnsafe #-}
elementwiseUnsafe f m m' = matrix (nrows m) (ncols m) $
  \(i,j) -> f (unsafeGet i j m) (unsafeGet i j m')


-------------------------------------------------------
-------------------------------------------------------
---- MATRIX MULTIPLICATION

{- $mult

Four methods are provided for matrix multiplication.

* 'multStd':
     Matrix multiplication following directly the definition.
     This is the best choice when you know for sure that your
     matrices are small.

* 'multStd2':
     Matrix multiplication following directly the definition.
     However, using a different definition from 'multStd'.
     According to our benchmarks with this version, 'multStd2' is
     around 3 times faster than 'multStd'.

* 'multStrassen':
     Matrix multiplication following the Strassen's algorithm.
     Complexity grows slower but also some work is added
     partitioning the matrix. Also, it only works on square
     matrices of order @2^n@, so if this condition is not
     met, it is zero-padded until this is accomplished.
     Therefore, its use is not recommended.

* 'multStrassenMixed':
     This function mixes the previous methods.
     It provides a better performance in general. Method @(@'*'@)@
     of the 'Num' class uses this function because it gives the best
     average performance. However, if you know for sure that your matrices are
     small (size less than 500x500), you should use 'multStd' or 'multStd2' instead,
     since 'multStrassenMixed' is going to switch to those functions anyway.

We keep researching how to get better performance for matrix multiplication.
If you want to be on the safe side, use ('*').

-}

-- | Standard matrix multiplication by definition.
multStd :: (Num a, Unbox a) => Matrix a -> Matrix a -> Matrix a
{-# INLINE multStd #-}
multStd a1@(M n m _) a2@(M n' m' _)
   -- Checking that sizes match...
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   | otherwise = multStd_ a1 a2


-- | Standard matrix multiplication by definition.
multStd2 :: (Num a, Unbox a) => Matrix a -> Matrix a -> Matrix a
{-# INLINE multStd2 #-}
multStd2 a1@(M n m _) a2@(M n' m' _)
   -- Checking that sizes match...
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   | otherwise = multStd__ a1 a2

-- | Standard matrix multiplication by definition, without checking if sizes match.
multStd_ :: (Num a, Unbox a) => Matrix a -> Matrix a -> Matrix a
{-# INLINE multStd_ #-}
multStd_ a@(M 1 1 _) b@(M 1 1 _) = M 1 1 $ V.singleton $ (a ! (1,1)) * (b ! (1,1))
multStd_ a@(M 2 2 _) b@(M 2 2 _) =
  M 2 2 $
    let -- A
        a11 = a !. (1,1) ; a12 = a !. (1,2)
        a21 = a !. (2,1) ; a22 = a !. (2,2)
        -- B
        b11 = b !. (1,1) ; b12 = b !. (1,2)
        b21 = b !. (2,1) ; b22 = b !. (2,2)
    in V.fromList
         [ a11*b11 + a12*b21 , a11*b12 + a12*b22
         , a21*b11 + a22*b21 , a21*b12 + a22*b22
           ]
multStd_ a@(M 3 3 _) b@(M 3 3 _) =
  M 3 3 $
    let -- A
        a11 = a !. (1,1) ; a12 = a !. (1,2) ; a13 = a !. (1,3)
        a21 = a !. (2,1) ; a22 = a !. (2,2) ; a23 = a !. (2,3)
        a31 = a !. (3,1) ; a32 = a !. (3,2) ; a33 = a !. (3,3)
        -- B
        b11 = b !. (1,1) ; b12 = b !. (1,2) ; b13 = b !. (1,3)
        b21 = b !. (2,1) ; b22 = b !. (2,2) ; b23 = b !. (2,3)
        b31 = b !. (3,1) ; b32 = b !. (3,2) ; b33 = b !. (3,3)
    in V.fromList
         [ a11*b11 + a12*b21 + a13*b31 , a11*b12 + a12*b22 + a13*b32 , a11*b13 + a12*b23 + a13*b33
         , a21*b11 + a22*b21 + a23*b31 , a21*b12 + a22*b22 + a23*b32 , a21*b13 + a22*b23 + a23*b33
         , a31*b11 + a32*b21 + a33*b31 , a31*b12 + a32*b22 + a33*b32 , a31*b13 + a32*b23 + a33*b33
           ]
multStd_ a@(M n m _) b@(M _ m' _) = matrix n m' $ \(i,j) -> sum [ a !. (i,k) * b !. (k,j) | k <- [0 .. pred m] ]


multStd__ :: (Num a, Unbox a) => Matrix a -> Matrix a -> Matrix a
{-# INLINE multStd__ #-}
multStd__ a b = matrix r c $ \(i,j) -> dotProduct (BV.unsafeIndex avs $ i - 1) (BV.unsafeIndex bvs $ j - 1)
  where
    r = nrows a
    avs = BV.generate r $ \i -> getRow (i+1) a
    c = ncols b
    bvs = BV.generate c $ \i -> getCol (i+1) b


dotProduct :: (Num a, Unbox a) => V.Vector a -> V.Vector a -> a
{-# INLINE dotProduct #-}
dotProduct v1 v2 = numLoopFold 0 (V.length v1 - 1) 0 $
  \r i -> V.unsafeIndex v1 i * V.unsafeIndex v2 i + r


-------------------------------------------------------
-------------------------------------------------------
---- NUMERICAL INSTANCE

instance (Num a, Unbox a) => Num (Matrix a) where
 fromInteger = M 1 1 . V.singleton . fromInteger
 negate = mapMat negate
 abs = mapMat abs
 signum = mapMat signum

 -- Addition of matrices.
 {-# SPECIALIZE (+) :: Matrix Double -> Matrix Double -> Matrix Double #-}
 {-# SPECIALIZE (+) :: Matrix Int -> Matrix Int -> Matrix Int #-}
 (+) = elementwise (+)

 -- Substraction of matrices.
 {-# SPECIALIZE (-) :: Matrix Double -> Matrix Double -> Matrix Double #-}
 {-# SPECIALIZE (-) :: Matrix Int -> Matrix Int -> Matrix Int #-}
 (-) = elementwise (-)

 -- Multiplication of matrices.
 {-# INLINE (*) #-}
 (*) = multStd2

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

-- | Scale a row by a given factor.
--   Example:
--
-- >              ( 1 2 3 )   (  1  2  3 )
-- >              ( 4 5 6 )   (  8 10 12 )
-- > scaleRow 2 2 ( 7 8 9 ) = (  7  8  9 )
scaleRow :: (Num a, Unbox a)
         => a -> Int -> Matrix a -> Matrix a
scaleRow = mapRow . const . (*)

-- | Add to one row a scalar multiple of another row.
--   Example:
--
-- >                   ( 1 2 3 )   (  1  2  3 )
-- >                   ( 4 5 6 )   (  6  9 12 )
-- > combineRows 2 2 1 ( 7 8 9 ) = (  7  8  9 )
combineRows :: (Num a, Unbox a)
            => Int -> a -> Int -> Matrix a -> Matrix a
combineRows r1 l r2 m =
  mapRow (\j x -> x + l * getElem r2 j m) r1 m

-- | Switch two rows of a matrix.
--   Example:
--
-- >                ( 1 2 3 )   ( 4 5 6 )
-- >                ( 4 5 6 )   ( 1 2 3 )
-- > switchRows 1 2 ( 7 8 9 ) = ( 7 8 9 )
switchRows :: (Unbox a)
           => Int -- ^ Row 1.
           -> Int -- ^ Row 2.
           -> Matrix a -- ^ Original matrix.
           -> Matrix a -- ^ Matrix with rows 1 and 2 switched.
switchRows r1 r2 (M n m vs) = M n m $ V.modify (\mv ->
  numLoop 0 (m-1) $ \j ->
    MV.swap mv (encode m (r1,j)) (encode m (r2,j))) vs

-- | Switch two coumns of a matrix.
--   Example:
--
-- >                ( 1 2 3 )   ( 2 1 3 )
-- >                ( 4 5 6 )   ( 5 4 6 )
-- > switchCols 1 2 ( 7 8 9 ) = ( 8 7 9 )
switchCols :: (Unbox a)
           => Int -- ^ Col 1.
           -> Int -- ^ Col 2.
           -> Matrix a -- ^ Original matrix.
           -> Matrix a -- ^ Matrix with cols 1 and 2 switched.
switchCols c1 c2 (M n m vs) = M n m $ V.modify (\mv ->
  numLoop 0 (n-1) $ \j ->
    MV.swap mv (encode m (j,c1)) (encode m (j,c2))) vs
