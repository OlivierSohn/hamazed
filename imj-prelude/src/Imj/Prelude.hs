{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Prelude
          ( safeMergeWithKey, safeMerge
          -- * Reexports
          , module Exported
          ) where

import           Prelude as Exported
  ( Eq, Show(..), Real, Num(..), Enum, Bounded, Integral, Fractional, Floating, RealFrac
  , Ord, Monoid(..), Monad(..), Functor, Read, Applicative, Foldable
  , Bool(..), Char, Float, Double, IO, Int, Maybe(..), Either(..), Ordering(..)
  , either
  , sum, map, concatMap, filter, mapM_, sequence_
  , all, any, notElem, null
  , (++), takeWhile, drop, reverse, iterate
  , zip, zip3, unzip, zipWith, fst, snd
  , fmap, (.), (=<<), ($), (<$>), const, id, flip, curry, uncurry
  , compare, not, or, (||), (&&), otherwise
  , (*), (**), (+), (-), (/), (^), (==), (/=), (>), (<), (>=), (<=)
  , realToFrac, fromIntegral, recip, signum, pred, succ
  , sin, cos, pi
  , mod, rem, min, max, abs, floor, round, ceiling, maxBound, minBound
  , negate, div, divMod, quot, quotRem, even, odd
  , error, undefined, seq
  , (!!), ($!)
  )

import           GHC.Generics as Exported(Generic)
import           Control.Applicative as Exported((<|>), pure, (<*>), (*>), (<*))
import           Control.Arrow as Exported((>>>))
import           Control.Exception as Exported(assert)
import           Control.DeepSeq as Exported(NFData(..), ($!!), deepseq, force)
import           Control.Monad as Exported(sequence, when, unless, void, (<=<), (>=>), zipWithM_, forever
                              , replicateM, replicateM_, foldM, foldM_, forM_, forM, join)
import           Control.Monad.IO.Class as Exported(liftIO)
import           Control.Monad.Reader as Exported(ReaderT)
import           Data.Bool as Exported(bool)
import           Data.Binary as Exported(Binary)
import           Data.List as Exported(cycle, repeat)
import           Data.Maybe as Exported(listToMaybe, maybeToList, fromMaybe, maybe, catMaybes, mapMaybe, isNothing)
import           Data.Map.Merge.Strict(merge, preserveMissing, zipWithMatched)
import           Data.Map.Strict(Map)
import           Data.Monoid as Exported((<>))
import           Data.Ratio as Exported((%))
import           Data.String as Exported(String)
import           Data.Text as Exported(Text)
import           Data.Traversable as Exported(mapM)
import           Data.Word as Exported(Word8)

import           Text.Show.Pretty as Exported(PrettyVal(..))

{-
-- | A replacement for 'Prelude.!!', that helps debugging.
(!!) :: (Integral n, Num n, Show a, Show n) => [a] -> n -> a
(!!) l i = go l i
  where
    go [] n = error $ "empty list" ++ show (l,i)
    go (x:xs) n | n == 0 = x
                | n < 0 = error $ "negative index" ++ show (x:xs, l, i)
                | otherwise = go xs (n-1)
-}


-- | Safe in the sense that it keeps elements that are not in the other map.
safeMerge :: (Ord a) => (b -> b -> b) -> Map a b -> Map a b -> Map a b
safeMerge f = safeMergeWithKey (const f)

-- | Same as 'safeMerge', but the key is passed to the merge function.
safeMergeWithKey :: (Ord a) => (a -> b -> b -> b) -> Map a b -> Map a b -> Map a b
safeMergeWithKey = merge preserveMissing preserveMissing . zipWithMatched
