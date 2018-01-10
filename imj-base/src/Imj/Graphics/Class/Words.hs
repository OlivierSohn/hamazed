{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Graphics.Class.Words
            ( Words(..)
            , SingleWord(..)
            ) where

import qualified Prelude(splitAt, length)

import           Imj.Prelude hiding(unwords, words)
import qualified Data.String as String(words, unwords)
import qualified Data.Text as Text(length, splitAt, words, unwords)

newtype SingleWord a = SingleWord a

-- | A 'Words' is a 'String'-like that can be split in words.
class Words a where
  -- | Produce a list of words from a 'Words'.
  words :: a -> [SingleWord a]
  -- | Consume a list of words to produce a 'Words'.
  unwords :: [SingleWord a] -> a
  -- | Return the number of characters in a 'Words'.
  length :: a -> Int
  -- | Split a 'SingleWord' in two (necessary for example when doing multiline layout, if
  -- a single word is bigger than the line, so that we can split it in multiple parts).
  splitAt :: Int -> SingleWord a -> (SingleWord a,SingleWord a)

  -- | Splits a 'Words' in multiple lines, respecting the words integrity
  -- when words are smaller than the line length.
  {-# INLINABLE multiLine #-}
  multiLine :: a
            -> Int
            -- ^ Line length.
            -> [a]
  multiLine str maxLineSize =
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
                let (cur,next) = splitAt maxLineSize x
                in toMultiLine' (next:xs) 0 [] ([cur] : curLines)
              else
                toMultiLine' a 0 [] (curLine : curLines)
          else
            toMultiLine' xs sz (x:curLine) curLines

instance Words ([] Char) where
  words = map SingleWord . String.words
  unwords = String.unwords . map (\(SingleWord w) -> w)
  length = Prelude.length
  splitAt n (SingleWord w) = (SingleWord w1, SingleWord w2)
    where (w1,w2) = Prelude.splitAt n w
  {-# INLINABLE words #-}
  {-# INLINABLE unwords #-}
  {-# INLINABLE length #-}
  {-# INLINABLE splitAt #-}

instance Words Text where
  words = map SingleWord . Text.words
  unwords = Text.unwords . map (\(SingleWord w) -> w)
  length = Text.length
  splitAt n (SingleWord w) = (SingleWord w1, SingleWord w2)
    where (w1,w2) = Text.splitAt n w
  {-# INLINABLE words #-}
  {-# INLINABLE unwords #-}
  {-# INLINABLE length #-}
  {-# INLINABLE splitAt #-}
