{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Geo.Discrete.Interleave
    ( InterleaveInfo(..)
    , mkInterleaveInfo
    , countUsefulInterleavedVariations
    , interleaveHalves
    , interleaveHalves'
    , interleaveIdx
    ) where

import           Imj.Prelude
import           Control.DeepSeq(NFData(..))
import           Control.Loop (numLoop)
import           Data.List(length, (!!))
import           Data.Vector.Binary()
import qualified Data.Vector.Storable         as S hiding(Storable)
import qualified Data.Vector.Storable.Mutable as MS

import           Imj.Util


data InterleaveInfo = InterleaveInfo {
    nUseful :: !Int
   -- ^ The count of useful variations
  , _lookup :: S.Vector Int
   -- ^ lookup vector to get interleaved indices
} deriving(Generic, Eq, Show, Ord)
instance Binary InterleaveInfo
instance NFData InterleaveInfo

mkInterleaveInfo :: Int
                 -- ^ the length of the array that will be interleaved
                 -> InterleaveInfo
mkInterleaveInfo n =
  InterleaveInfo len v
 where
  (len,interleave) = getInterleavedInfos n

  v = S.create $ do
    mv <- MS.unsafeNew n
    numLoop 0 (n-1) $ \i ->
      MS.unsafeWrite mv i $ interleave i
    return mv

getInterleavedInfos :: Int
                    -- ^ the length of the array that will be interleaved
                    -> (Int, Int -> Int)
                    -- ^ fst: The count of useful variations
                    -- , snd : the function to get interleaved indices
getInterleavedInfos !d =
  let !iD = mkInterleaveData d
  in (countUsefulInterleavedVariations d, interleaveIdx iD)


{- Definition: two interleaved variations are /equivalent/ if we can transform
one into the other by using reverse and list rotations.

In the cycle of lists formed by applying 'interleaveHalves' repeatedly on an input list,
there are n lists that are /useful wrt topology/, i.e no 2 of these n lists are equivalent,
and every subsequent list in the cycle has an equivalent among these n lists.
(Note that once we encounter an equivalent representation,
all subsequent representations are equivalent to an already encoutered one.)
-}
{-# INLINE countUsefulInterleavedVariations #-}
countUsefulInterleavedVariations :: Int -> Int
-- See 'testInterleaveHalves' on how to generate these values:
countUsefulInterleavedVariations   1 =   1
countUsefulInterleavedVariations   2 =   1
countUsefulInterleavedVariations   3 =   1
countUsefulInterleavedVariations   4 =   2
countUsefulInterleavedVariations   5 =   2
countUsefulInterleavedVariations   6 =   3
countUsefulInterleavedVariations   7 =   3
countUsefulInterleavedVariations   8 =   3
countUsefulInterleavedVariations   9 =   3
countUsefulInterleavedVariations  10 =   5
countUsefulInterleavedVariations  11 =   5
countUsefulInterleavedVariations  12 =   6
countUsefulInterleavedVariations  13 =   6
countUsefulInterleavedVariations  14 =   4
countUsefulInterleavedVariations  15 =   4
countUsefulInterleavedVariations  16 =   4
countUsefulInterleavedVariations  17 =   4
countUsefulInterleavedVariations  18 =   9
countUsefulInterleavedVariations  19 =   9
countUsefulInterleavedVariations  20 =   6
countUsefulInterleavedVariations  21 =   6
countUsefulInterleavedVariations  22 =  11
countUsefulInterleavedVariations  23 =  11
countUsefulInterleavedVariations  24 =  10
countUsefulInterleavedVariations  25 =  10
countUsefulInterleavedVariations  26 =   9
countUsefulInterleavedVariations  27 =   9
countUsefulInterleavedVariations  28 =  14
countUsefulInterleavedVariations  29 =  14
countUsefulInterleavedVariations  30 =   5
countUsefulInterleavedVariations  31 =   5
countUsefulInterleavedVariations  32 =   5
countUsefulInterleavedVariations  33 =   5
countUsefulInterleavedVariations  34 =  12
countUsefulInterleavedVariations  35 =  12
countUsefulInterleavedVariations  36 =  18
countUsefulInterleavedVariations  37 =  18
countUsefulInterleavedVariations  38 =  12
countUsefulInterleavedVariations  39 =  12
countUsefulInterleavedVariations  40 =  10
countUsefulInterleavedVariations  41 =  10
countUsefulInterleavedVariations  42 =   7
countUsefulInterleavedVariations  43 =   7
countUsefulInterleavedVariations  44 =  12
countUsefulInterleavedVariations  45 =  12
countUsefulInterleavedVariations  46 =  23
countUsefulInterleavedVariations  47 =  23
countUsefulInterleavedVariations  48 =  21
countUsefulInterleavedVariations  49 =  21
countUsefulInterleavedVariations  50 =   8
countUsefulInterleavedVariations  51 =   8
countUsefulInterleavedVariations  52 =  26
countUsefulInterleavedVariations  53 =  26
countUsefulInterleavedVariations  54 =  20
countUsefulInterleavedVariations  55 =  20
countUsefulInterleavedVariations  56 =   9
countUsefulInterleavedVariations  57 =   9
countUsefulInterleavedVariations  58 =  29
countUsefulInterleavedVariations  59 =  29
countUsefulInterleavedVariations  60 =  30
countUsefulInterleavedVariations  61 =  30
countUsefulInterleavedVariations  62 =   6
countUsefulInterleavedVariations  63 =   6
countUsefulInterleavedVariations  64 =   6
countUsefulInterleavedVariations  65 =   6
countUsefulInterleavedVariations  66 =  33
countUsefulInterleavedVariations  67 =  33
countUsefulInterleavedVariations  68 =  22
countUsefulInterleavedVariations  69 =  22
countUsefulInterleavedVariations  70 =  35
countUsefulInterleavedVariations  71 =  35
countUsefulInterleavedVariations  72 =   9
countUsefulInterleavedVariations  73 =   9
countUsefulInterleavedVariations  74 =  20
countUsefulInterleavedVariations  75 =  20
countUsefulInterleavedVariations  76 =  30
countUsefulInterleavedVariations  77 =  30
countUsefulInterleavedVariations  78 =  39
countUsefulInterleavedVariations  79 =  39
countUsefulInterleavedVariations  80 =  27
countUsefulInterleavedVariations  81 =  27
countUsefulInterleavedVariations  82 =  41
countUsefulInterleavedVariations  83 =  41
countUsefulInterleavedVariations  84 =   8
countUsefulInterleavedVariations  85 =   8
countUsefulInterleavedVariations  86 =  28
countUsefulInterleavedVariations  87 =  28
countUsefulInterleavedVariations  88 =  11
countUsefulInterleavedVariations  89 =  11
countUsefulInterleavedVariations  90 =  12
countUsefulInterleavedVariations  91 =  12
countUsefulInterleavedVariations  92 =  10
countUsefulInterleavedVariations  93 =  10
countUsefulInterleavedVariations  94 =  36
countUsefulInterleavedVariations  95 =  36
countUsefulInterleavedVariations  96 =  24
countUsefulInterleavedVariations  97 =  24
countUsefulInterleavedVariations  98 =  15
countUsefulInterleavedVariations  99 =  15
countUsefulInterleavedVariations 100 =  50
countUsefulInterleavedVariations l = logBase2 l

-- | Interleave the first half with the second half.
interleaveHalves :: [a] -> [a]
interleaveHalves l =
  uncurry (++) $ go [] [] l
 where
  go l1 l2 []         = (   l1,l2)
  go l1 l2 [x1]       = (x1:l1,l2)
  go l1 l2 (x1:x2:xs) = go (x1:l1) (x2:l2) xs

-- | in a more imperative-style than interleaveHalves, with the same outcome
interleaveHalves' :: [a] -> [a]
interleaveHalves' l =
  map (\i -> l !! i) modIndices
 where
  len = length l
  indices = [0..len-1]
  inter = mkInterleaveData len
  modIndices = map
    (interleaveIdx inter)
    indices

data InterleaveData = InterleaveData {
    _firstOddIdx, _halfLength :: {-# UNPACK #-} !Int
}

mkInterleaveData :: Int -> InterleaveData
mkInterleaveData len =
  InterleaveData firstOddIdx halfLength
 where
  (halfLength, r)  = quotRem len 2
  firstOddIdx = halfLength + bool 1 0 (r == 0)

{-# INLINE interleaveIdx #-}
interleaveIdx :: InterleaveData -> Int -> Int
interleaveIdx (InterleaveData firstOddIdx halfLength) !i =
  let i' = firstOddIdx-i
  in if i' > 0
      then
        2 * (i' - 1)
      else
        2 * (halfLength + i') - 1
