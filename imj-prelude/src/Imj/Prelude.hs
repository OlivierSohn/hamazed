{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Prelude
          ( module Exported
          ) where

import           Prelude as Exported
  ( Eq, Show(..), Real, Num, Enum, Bounded, Integral, Ord, Monoid(..), Monad(..)
  , Functor, Read, Applicative, Foldable
  , Bool(..), Char, Float, Double, IO, Int, Maybe(..), Either(..), Ordering(..)
  , either
  , sum, map, concatMap, concat, filter, mapM_
  , all, any, notElem, null, minimum, maximum
  , replicate, (++), take, takeWhile, tail, last, head, drop, reverse, iterate
  , zip, zip3, unzip, zipWith, fst, snd
  , fmap, (.), (=<<), ($), (<$>), const, id, flip, curry, uncurry
  , compare, not, or, (||), (&&), otherwise
  , (*), (**), (+), (-), (/), (^), (==), (/=), (>), (<), (>=), (<=)
  , realToFrac, fromIntegral, recip, signum, pred, succ
  , sin, cos, pi
  , mod, min, max, abs, floor, round, ceiling, maxBound, minBound
  , negate, div, divMod, quot, quotRem, even, odd
  , error, undefined
  , (!!), ($!)
  )

import           GHC.Generics as Exported(Generic)
import           Control.Applicative as Exported((<|>), pure, (<*>), (*>), (<*))
import           Control.Arrow as Exported((>>>))
import           Control.Exception as Exported(assert)
import           Control.Monad as Exported(sequence, when, unless, void, (<=<), (>=>), zipWithM_, forever
                              , replicateM, replicateM_, foldM, foldM_, forM_, forM, join)
import           Control.Monad.IO.Class as Exported(liftIO)
import           Control.Monad.Reader as Exported(ReaderT)
import           Data.Bool as Exported(bool)
import           Data.Binary as Exported(Binary)
import           Data.List as Exported(intercalate, cycle, repeat, words, unwords)
import           Data.Maybe as Exported(listToMaybe, fromMaybe, maybe, catMaybes, mapMaybe, isNothing)
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
