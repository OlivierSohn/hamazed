{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Graphics.Text.Render
    ( -- * Render utilities
      showListOrSingleton
    , showArray
    , showArrayN
    , indexedShowArray
    , indexedShowArrayN
    , showInBox
    , showInBox'
    , addRight
    , justifyR
    , justifyL
    ) where

import           Imj.Prelude

import           Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IMap
import qualified Data.List as List
import           Data.Maybe(listToMaybe)
import           Data.String(IsString(..))
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

showInBoxBase :: (Characters s) => Char -> Char -> [s] -> [s]
showInBoxBase low up l =
  bar up : map (withFrame '|') l ++ [bar low]
 where
  bar = replicate (maxWidth + 2)
  withFrame f str = cons f (justifyL maxWidth str) <> fromString [f]
  maxWidth = maxL l

showInBox :: (Characters s) => [s] -> [s]
showInBox = showInBoxBase '-' '-'

showInBox' :: (Characters s) => [s] -> [s]
showInBox' = showInBoxBase 'T' '_'

showArray :: (Characters s) => Maybe (s, s) -> [(s,s)] -> [s]
showArray a = mconcat . IMap.elems . indexedShowArray a . IMap.fromDistinctAscList . zip [0..]

indexedShowArray :: (Characters s) => Maybe (s, s) -> IntMap (s,s) -> IntMap [s]
indexedShowArray mayTitles body =
  indexedShowArrayN
    (fmap pairToList mayTitles)
    (IMap.map pairToList body)
 where pairToList (a,b) = [a,b]

showArrayN :: (Characters s)
           => Maybe [s]
           -- ^ The column titles
           -> [[s]]
           -- ^ The body, row by row
           -> [s]
showArrayN t = mconcat . IMap.elems . indexedShowArrayN t . IMap.fromDistinctAscList . zip [0..]

indexedShowArrayN :: (Characters s)
                  => Maybe [s]
                  -- ^ The column titles
                  -> IntMap [s]
                  -- ^ The body, row by row, where the list contains each column value
                  -> IntMap [s]
                  -- ^ The result, where the list contains potentially several rows.
indexedShowArrayN mayTitles body =
  mconcat
    [ IMap.singleton headerIndex $
        maybe [] ((:) bar . format . (:[])) mayTitles ++
        [bar]
    , IMap.fromDistinctAscList $
          zip (IMap.keys body) $
            map (:[]) $ format bodyElems -- in the future, format may be [s] -> [[s]]
    , IMap.singleton footerIndex [bar]
    ]
 where
  headerIndex = bool (pred $ fst $ IMap.findMin body) 0 $ IMap.null body -- TODO use lookupMin when containers 0.5.11 is available
  footerIndex = bool (succ $ fst $ IMap.findMax body) 1 $ IMap.null body

  bodyElems = IMap.elems body

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
  arrayLines = maybe id (:) mayTitles bodyElems
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
