{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Graphics.Text.Render
    ( -- * Render utilities
      showListOrSingleton
    , showArray
    , showArrayN
    , showInBox
    , addRight
    , justifyR
    , justifyL
    ) where

import           Imj.Prelude

import qualified Data.List as List
import           Data.String(IsString(..))
import           Data.Maybe(listToMaybe)
import           Data.Text(Text, pack)

import           Imj.Graphics.Class.Words
import           Imj.Util

addRight :: (Characters s) => [s] -> Int -> [s] -> [s]
addRight l1' margin l2' =
  zipWith (<>)
    (fillH l1 maxWidth1)
    (fillH l2 maxWidth2)
 where
  fillH x maxWidth = List.take height $ x ++ List.repeat (replicate maxWidth ' ')
  maxWidth1 = maxL l1'
  maxWidth2 = maxL l2'
  height = max (List.length l1') (List.length l2')
  fillW maxWidth = map (justifyL maxWidth)
  l1 = fillW (maxWidth1 + margin) l1'
  l2 = fillW maxWidth2 l2'

maxL :: (Characters s) => [s] -> Int
maxL = fromMaybe 0 . maximumMaybe . map length

showInBox :: (Characters s) => [s] -> [s]
showInBox l =
  bar '_' : map (withFrame '|') l ++ [bar 'T']
 where
  bar = replicate (maxWidth + 2)
  withFrame f str = cons f (justifyL maxWidth str) <> fromString [f]
  maxWidth = maxL l

showArray :: (Characters s) => Maybe (s, s) -> [(s,s)] -> [s]
showArray mayTitles body =
  showArrayN
    (fmap pairToList mayTitles)
    (map pairToList body)
 where pairToList (a,b) = [a,b]

showArrayN :: (Characters s) => Maybe [s] -> [[s]] -> [s]
showArrayN mayTitles body =
  maybe mempty (\titles -> bar : format [titles]) mayTitles
   <> [bar] <> format body <> [bar]
 where
  bar = replicate lBar '-'
  lBar = maxL $ format arrayLines
  format =
    map
      (\strs ->
      "| " <>
      intercalate
        " | "
        (map
          (\(columnIdx, str, justify) -> justify (ls !! columnIdx) str)
          $ zip3 [0..] strs justifications) <>
      " |")
  arrayLines = maybe body (:body) mayTitles
  ls = map
    (\colIdx -> maxL $ mapMaybe (listToMaybe . drop colIdx) arrayLines)
    [0..]

  justifications = justifyL : repeat justifyR

{-# INLINE justifyL #-}
{-# INLINE justifyR #-}
justifyR, justifyL :: (Characters s) => Int -> s -> s
justifyR n x =
  replicate (n-length x) ' ' <> x
justifyL n x =
  x <> replicate (n-length x) ' '

{-# INLINABLE showListOrSingleton #-}
-- | If list is a singleton, show the element, else show the list.
showListOrSingleton :: Show a => [a] -> Text
showListOrSingleton [e] = pack $ show e
showListOrSingleton l   = pack $ show l
