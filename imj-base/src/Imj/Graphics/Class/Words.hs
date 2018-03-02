{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Imj.Graphics.Class.Words
            ( Words(..)
            , Characters(..)
            , SingleWord(..)
            , multiLineTrivial
            ) where

import qualified Prelude(splitAt, length)

import           Imj.Prelude hiding(unwords, words)

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Control.Monad( zipWithM_ )
import qualified Data.String as String(words, unwords)
import qualified Data.Text as Text(length, splitAt, words, unwords, null, unpack)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Color
import           Imj.Graphics.Font


newtype SingleWord a = SingleWord a

class Characters a where
  length :: a -> Int
  empty :: a -> Bool
  splitAt :: Int -> a -> (a, a)
  drawOnPath :: (MonadIO m, MonadReader e m, Draw e)
             => [Coords Pos] -> a -> m ()

-- | A 'Words' is a 'Characters' that can be split in words.
class (Characters a) => Words a where
  -- | Produce a list of words from a 'Words'.
  words :: a -> [SingleWord a]
  -- | Consume a list of words to produce a 'Words'.
  unwords :: [SingleWord a] -> a

  -- | Splits a 'Words' in multiple lines, respecting the words integrity
  -- when words are smaller than the line length.
  {-# INLINABLE multiLine #-}
  multiLine :: Int
            -- ^ Line length.
            -> a
            -> [a]
  multiLine maxLineSize str  =
    map (unwords . reverse) $ reverse $ toMultiLine' (words str) 0 [] []
   where
    toMultiLine' :: (Words a)
                 => [SingleWord a] -> Int -> [SingleWord a] -> [[SingleWord a]] -> [[SingleWord a]]
    toMultiLine' [] _ []      curLines = curLines
    toMultiLine' [] _ curLine curLines = curLine : curLines
    toMultiLine' a@(x@(SingleWord w):xs) curLineSize curLine curLines =
      let l = length w
          sz = 1 + l + curLineSize
      in if sz > maxLineSize
          then
            if curLineSize == 0
              then
                -- split the word
                let (cur,next) = splitAt maxLineSize w
                in toMultiLine' (SingleWord next:xs) 0 [] ([SingleWord cur] : curLines)
              else
                toMultiLine' a 0 [] (curLine : curLines)
          else
            toMultiLine' xs sz (x:curLine) curLines

{-# INLINABLE multiLineTrivial #-}
multiLineTrivial :: (Words a)
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
  splitAt = Prelude.splitAt
  drawOnPath positions str = do
    d <- asks drawGlyph'
    zipWithM_ (\pos char -> d (textGlyph char) pos whiteOnBlack) positions str
  {-# INLINABLE length #-}
  {-# INLINABLE splitAt #-}
  {-# INLINABLE empty #-}
  {-# INLINABLE drawOnPath #-}

instance Words ([] Char) where
  words = map SingleWord . String.words
  unwords = String.unwords . map (\(SingleWord w) -> w)
  {-# INLINABLE words #-}
  {-# INLINABLE unwords #-}

instance Characters Text where
  length = Text.length
  empty = Text.null
  splitAt = Text.splitAt
  drawOnPath positions txt = drawOnPath positions (Text.unpack txt)
  {-# INLINABLE length #-}
  {-# INLINABLE splitAt #-}
  {-# INLINABLE empty #-}
  {-# INLINABLE drawOnPath #-}

instance Words Text where
  words = map SingleWord . Text.words
  unwords = Text.unwords . map (\(SingleWord w) -> w)
  {-# INLINABLE words #-}
  {-# INLINABLE unwords #-}
