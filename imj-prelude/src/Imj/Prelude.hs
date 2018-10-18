{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Prelude
          ( safeMergeWithKey, safeMerge, safeZipMerge
          , fmapM
          -- * Reexports
          , module Exported
          ) where

import           Prelude as Exported
  ( Eq, Show(..), Real, Num(..), Enum(..), Bounded, Integral, Fractional, Floating, RealFrac
  , Ord, Monoid(..), Monad(..), Functor, Read, Applicative, Foldable
  , Bool(..), Char, Float, Double, IO, Int, Maybe(..), Either(..), Ordering(..), FilePath
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
import           Control.Applicative as Exported(Alternative(..), (<|>), pure, (<*>), (*>), (<*))
import           Control.Arrow as Exported((>>>))
import           Control.Exception as Exported(assert)
import           Control.DeepSeq as Exported(NFData(..), ($!!), deepseq, force)
import           Control.Monad as Exported(sequence, when, unless, void, (<=<), (>=>), zipWithM_, forever
                              , replicateM, replicateM_, foldM, foldM_, forM_, forM, join)
import           Control.Monad.IO.Class as Exported(MonadIO, liftIO)
import           Control.Monad.Reader as Exported(ReaderT)
import           Data.Bool as Exported(bool)
import           Data.Binary as Exported(Binary)
import           Data.Either as Exported(lefts)
import           Data.Hashable as Exported(Hashable(..), hashUsing)
import           Data.List as Exported(cycle, repeat)
import           Data.Maybe as Exported(listToMaybe, maybeToList, fromMaybe, maybe, catMaybes, mapMaybe, isNothing)
import           Data.Map.Merge.Strict
import           Data.Map.Strict(Map)
import           Data.Ratio as Exported((%))
import           Data.Semigroup as Exported(Semigroup (..))
import           Data.String as Exported(String)
import           Data.Text as Exported(Text)
import           Data.Traversable as Exported(mapM)
import           Data.Word as Exported(Word8)
import           Language.Haskell.TH.Syntax as Exported(Lift)

import           Text.Show.Pretty as Exported(PrettyVal(..))

import           Imj.Data.List as Exported

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

-- | Zips two maps with the same key type but different elements types.
-- All elements are preserved.
{-# INLINABLE safeZipMerge #-}
safeZipMerge :: (Ord a) => Map a b -> Map a c -> Map a (Maybe b, Maybe c)
safeZipMerge = merge
  (mapMissing (\_ x -> (Just x, Nothing)))
  (mapMissing (\_ y -> (Nothing, Just y)))
  (zipWithMatched (\_ x y -> (Just x, Just y)))

{-# INLINE fmapM #-}
fmapM :: Monad m => (a -> m ()) -> Maybe a -> m ()
fmapM f v = maybe (return ()) f v
