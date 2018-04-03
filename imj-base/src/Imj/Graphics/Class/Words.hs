{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Imj.Graphics.Class.Words
            ( Characters(..)
            , multiLineTrivial
            , multiLine
            ) where

import qualified Prelude(splitAt, length)

import           Imj.Prelude hiding(unwords, words)

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Control.Monad( zipWithM_ )
import           Data.String(IsString(..))
import qualified Data.List as List
import qualified Data.String as String(words, unwords)
import qualified Data.Text as Text

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Color
import           Imj.Graphics.Font


class (IsString a, Monoid a) => Characters a where
  length :: a -> Int
  empty :: a -> Bool
  cons :: Char -> a -> a
  intercalate :: a -> [a] -> a
  take :: Int -> a -> a
  concat :: [a] -> a
  splitAt :: Int -> a -> (a, a)

  colorize :: LayeredColor -> a -> a

  drawOnPath :: (MonadIO m, MonadReader e m, Draw e)
             => [Coords Pos] -> a -> m ()

  replicate :: Int -> Char -> a
  replicate n c = fromString (List.replicate n c)

  -- | Produce a list of words
  words :: a -> [a]
  -- | Consume a list of words
  unwords :: [a] -> a


-- | Splits a 'Words' in multiple lines, respecting the words integrity
-- when words are smaller than the line length.
{-# INLINABLE multiLine #-}
multiLine :: (Characters a)
          => Int
          -- ^ Line length.
          -> a
          -> [a]
multiLine maxLineSize str  =
  map (unwords . reverse) $ reverse $ toMultiLine' (words str) 0 [] []
 where
  toMultiLine' :: (Characters a)
               => [a] -> Int -> [a] -> [[a]] -> [[a]]
  toMultiLine' [] _ []      curLines = curLines
  toMultiLine' [] _ curLine curLines = curLine : curLines
  toMultiLine' a@(x@w:xs) curLineSize curLine curLines =
    let l = length w
        sz = 1 + l + curLineSize
    in if sz > maxLineSize
        then
          if curLineSize == 0
            then
              -- split the word
              let (cur,next) = splitAt maxLineSize w
              in toMultiLine' (next:xs) 0 [] ([cur] : curLines)
            else
              toMultiLine' a 0 [] (curLine : curLines)
        else
          toMultiLine' xs sz (x:curLine) curLines

{-# INLINABLE multiLineTrivial #-}
multiLineTrivial :: (Characters a)
                 => Length Width
                 -- ^ Line length.
                 -> a
                 -> [a]
multiLineTrivial n alltxt =
  let go l txt =
        let (oneLine, theRest) = splitAt (fromIntegral n) txt
        in if empty oneLine
              then l
              else go (oneLine:l) theRest
  in reverse $ go [] alltxt

instance Characters ([] Char) where
  length = Prelude.length
  empty = null
  cons = (:)
  intercalate = List.intercalate
  take = List.take
  concat = List.concat
  splitAt = Prelude.splitAt
  words = String.words
  unwords = String.unwords
  colorize _ = id
  drawOnPath positions str = do
    d <- asks drawGlyph'
    zipWithM_ (\pos char -> d (textGlyph char) pos whiteOnBlack) positions str
  {-# INLINABLE intercalate #-}
  {-# INLINABLE take #-}
  {-# INLINABLE concat #-}
  {-# INLINABLE cons #-}
  {-# INLINABLE length #-}
  {-# INLINABLE splitAt #-}
  {-# INLINABLE empty #-}
  {-# INLINABLE drawOnPath #-}
  {-# INLINABLE words #-}
  {-# INLINABLE unwords #-}

instance Characters Text where
  length = Text.length
  empty = Text.null
  cons = Text.cons
  intercalate = Text.intercalate
  splitAt = Text.splitAt
  concat = Text.concat
  take = Text.take
  colorize _ = id
  words = Text.words
  unwords = Text.unwords
  drawOnPath positions txt = drawOnPath positions (Text.unpack txt)
  {-# INLINABLE take #-}
  {-# INLINABLE concat #-}
  {-# INLINABLE intercalate #-}
  {-# INLINABLE cons #-}
  {-# INLINABLE length #-}
  {-# INLINABLE splitAt #-}
  {-# INLINABLE empty #-}
  {-# INLINABLE drawOnPath #-}
  {-# INLINABLE words #-}
  {-# INLINABLE unwords #-}
