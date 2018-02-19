{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Imj.Graphics.Class.Words
            ( Words(..)
            , SingleWord(..)
            , multiLineTrivial
            ) where

import qualified Prelude(splitAt, length)

import           Imj.Prelude hiding(unwords, words)
import qualified Data.String as String(words, unwords)
import qualified Data.Text as Text(length, splitAt, words, unwords, null)

import           Imj.Geo.Discrete.Types


newtype SingleWord a = SingleWord a

-- | A 'Words' is a 'String'-like that can be split in words.
class Words a where
  -- | Produce a list of words from a 'Words'.
  words :: a -> [SingleWord a]
  -- | Consume a list of words to produce a 'Words'.
  unwords :: [SingleWord a] -> a
  -- | Return the number of characters in a 'Words'.
  length :: a -> Int
  empty :: a -> Bool

  splitAt :: Int -> a -> (a, a)

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

instance Words ([] Char) where
  words = map SingleWord . String.words
  unwords = String.unwords . map (\(SingleWord w) -> w)
  length = Prelude.length
  empty = null
  splitAt = Prelude.splitAt
  {-# INLINABLE words #-}
  {-# INLINABLE unwords #-}
  {-# INLINABLE length #-}
  {-# INLINABLE splitAt #-}

instance Words Text where
  words = map SingleWord . Text.words
  unwords = Text.unwords . map (\(SingleWord w) -> w)
  length = Text.length
  empty = Text.null
  splitAt = Text.splitAt
  {-# INLINABLE words #-}
  {-# INLINABLE unwords #-}
  {-# INLINABLE length #-}
  {-# INLINABLE splitAt #-}
