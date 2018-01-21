{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

-- Initially I needed this custom Prelude to hide putStr and putChar,
-- since I provide equivalent functions that should be used instead to
-- render the game.
--
-- I find it also usefull to default-import functions that I use a lot.

module Imj.Prelude
          ( module Prelude
          , module Control.Applicative
          , module Control.Arrow
          , module Control.Exception
          , module Control.Monad
          , module Control.Monad.IO.Class
          , module Control.Monad.Reader
          , module Data.List
          , module Data.Maybe
          , module Data.Monoid
          , module Data.Ratio
          , module Data.String
          , module Data.Text
          , module Data.Word
          ) where

import           Prelude( Eq, Show(..), Real, Num, Enum, Bounded, Integral, Ord, Monoid(..), Monad(..)
                        , Functor, Read, Applicative
                        , Bool(..), Char, Float, Double, IO, Int, Maybe(..), Either(..), Ordering(..)
                        , either, maybe
                        , sum, map, concatMap, concat, filter, mapM, mapM_
                        , all, any, notElem, null, minimum, maximum
                        , replicate, (++), take, takeWhile, tail, last, head, drop, reverse, iterate, unwords
                        , zip, zipWith, fst, snd
                        , fmap, (.), (=<<), ($), (<$>), const, id, flip, curry, uncurry
                        , compare, not, or, (||), (&&), otherwise
                        , (*), (**), (+), (-), (/), (^), (==), (/=), (>), (<), (>=), (<=)
                        , realToFrac, fromIntegral, fromRational, recip, signum, pred, succ
                        , sin, cos, pi
                        , mod, min, max, abs, floor, round, ceiling, maxBound, minBound
                        , negate, div, divMod, quot, quotRem, even, odd
                        , error, undefined
                        , (!!)
                        )

import           Control.Applicative((<|>), pure)
import           Control.Arrow((>>>))
import           Control.Exception(assert)
import           Control.Monad(sequence, when, unless, void, (<=<), (>=>), Monad, zipWithM_, forever
                              , replicateM)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Reader(ReaderT)
import           Data.List(intercalate, cycle, repeat, words, unwords)
import           Data.Maybe(listToMaybe, fromMaybe, maybe, catMaybes, mapMaybe, isNothing)
import           Data.Monoid((<>))
import           Data.Ratio((%))
import           Data.String(String)
import           Data.Text(Text)
import           Data.Word(Word8)
