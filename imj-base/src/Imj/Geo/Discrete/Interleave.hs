{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Discrete.Interleave
    ( mkInterleaveData
    , countUsefullInterleavedVariations
    , interleaveHalves
    , interleaveHalves'
    , interleaveIdx
    ) where

import           Imj.Prelude

import           Data.List(length, (!!))
import           Imj.Util

{- Definition: two interleaved variations are /equivalent/ if we can transform
one into the other by using reverse and list rotations.

In the cycle of lists formed by applying 'interleaveHalves' repeatedly on an input list,
there are n lists that are /useful wrt topology/, i.e no 2 of these n lists are equivalent,
and every subsequent list in the cycle has an equivalent among these n lists.
(Note that once we encounter an equivalent representation,
all subsequent representations are equivalent to an already encoutered one.)
-}
{-# INLINE countUsefullInterleavedVariations #-}
countUsefullInterleavedVariations :: Int -> Int
-- See 'testInterleaveHalves' on how to generate these values:
countUsefullInterleavedVariations   1 =   1
countUsefullInterleavedVariations   2 =   1
countUsefullInterleavedVariations   3 =   1
countUsefullInterleavedVariations   4 =   2
countUsefullInterleavedVariations   5 =   2
countUsefullInterleavedVariations   6 =   3
countUsefullInterleavedVariations   7 =   3
countUsefullInterleavedVariations   8 =   3
countUsefullInterleavedVariations   9 =   3
countUsefullInterleavedVariations  10 =   5
countUsefullInterleavedVariations  11 =   5
countUsefullInterleavedVariations  12 =   6
countUsefullInterleavedVariations  13 =   6
countUsefullInterleavedVariations  14 =   4
countUsefullInterleavedVariations  15 =   4
countUsefullInterleavedVariations  16 =   4
countUsefullInterleavedVariations  17 =   4
countUsefullInterleavedVariations  18 =   9
countUsefullInterleavedVariations  19 =   9
countUsefullInterleavedVariations  20 =   6
countUsefullInterleavedVariations  21 =   6
countUsefullInterleavedVariations  22 =  11
countUsefullInterleavedVariations  23 =  11
countUsefullInterleavedVariations  24 =  10
countUsefullInterleavedVariations  25 =  10
countUsefullInterleavedVariations  26 =   9
countUsefullInterleavedVariations  27 =   9
countUsefullInterleavedVariations  28 =  14
countUsefullInterleavedVariations  29 =  14
countUsefullInterleavedVariations  30 =   5
countUsefullInterleavedVariations  31 =   5
countUsefullInterleavedVariations  32 =   5
countUsefullInterleavedVariations  33 =   5
countUsefullInterleavedVariations  34 =  12
countUsefullInterleavedVariations  35 =  12
countUsefullInterleavedVariations  36 =  18
countUsefullInterleavedVariations  37 =  18
countUsefullInterleavedVariations  38 =  12
countUsefullInterleavedVariations  39 =  12
countUsefullInterleavedVariations  40 =  10
countUsefullInterleavedVariations  41 =  10
countUsefullInterleavedVariations  42 =   7
countUsefullInterleavedVariations  43 =   7
countUsefullInterleavedVariations  44 =  12
countUsefullInterleavedVariations  45 =  12
countUsefullInterleavedVariations  46 =  23
countUsefullInterleavedVariations  47 =  23
countUsefullInterleavedVariations  48 =  21
countUsefullInterleavedVariations  49 =  21
countUsefullInterleavedVariations  50 =   8
countUsefullInterleavedVariations  51 =   8
countUsefullInterleavedVariations  52 =  26
countUsefullInterleavedVariations  53 =  26
countUsefullInterleavedVariations  54 =  20
countUsefullInterleavedVariations  55 =  20
countUsefullInterleavedVariations  56 =   9
countUsefullInterleavedVariations  57 =   9
countUsefullInterleavedVariations  58 =  29
countUsefullInterleavedVariations  59 =  29
countUsefullInterleavedVariations  60 =  30
countUsefullInterleavedVariations  61 =  30
countUsefullInterleavedVariations  62 =   6
countUsefullInterleavedVariations  63 =   6
countUsefullInterleavedVariations  64 =   6
countUsefullInterleavedVariations  65 =   6
countUsefullInterleavedVariations  66 =  33
countUsefullInterleavedVariations  67 =  33
countUsefullInterleavedVariations  68 =  22
countUsefullInterleavedVariations  69 =  22
countUsefullInterleavedVariations  70 =  35
countUsefullInterleavedVariations  71 =  35
countUsefullInterleavedVariations  72 =   9
countUsefullInterleavedVariations  73 =   9
countUsefullInterleavedVariations  74 =  20
countUsefullInterleavedVariations  75 =  20
countUsefullInterleavedVariations  76 =  30
countUsefullInterleavedVariations  77 =  30
countUsefullInterleavedVariations  78 =  39
countUsefullInterleavedVariations  79 =  39
countUsefullInterleavedVariations  80 =  27
countUsefullInterleavedVariations  81 =  27
countUsefullInterleavedVariations  82 =  41
countUsefullInterleavedVariations  83 =  41
countUsefullInterleavedVariations  84 =   8
countUsefullInterleavedVariations  85 =   8
countUsefullInterleavedVariations  86 =  28
countUsefullInterleavedVariations  87 =  28
countUsefullInterleavedVariations  88 =  11
countUsefullInterleavedVariations  89 =  11
countUsefullInterleavedVariations  90 =  12
countUsefullInterleavedVariations  91 =  12
countUsefullInterleavedVariations  92 =  10
countUsefullInterleavedVariations  93 =  10
countUsefullInterleavedVariations  94 =  36
countUsefullInterleavedVariations  95 =  36
countUsefullInterleavedVariations  96 =  24
countUsefullInterleavedVariations  97 =  24
countUsefullInterleavedVariations  98 =  15
countUsefullInterleavedVariations  99 =  15
countUsefullInterleavedVariations 100 =  50
countUsefullInterleavedVariations l = logBase2 l

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
interleaveIdx (InterleaveData firstOddIdx halfLength) i =
  let i' = firstOddIdx-i
  in if i' > 0
      then
        2 * (i' - 1)
      else
        2 * (halfLength + i') - 1
