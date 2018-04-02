{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Graphics.Text.Render
    ( -- * distribution utilities
      asDistribution
    , Distribution
    -- * Render utilities
    , showListOrSingleton
    , showArray
    , showArrayN
    , showDistribution
    , showQuantities
    , showQuantities'
    , showQuantities''
    , showInBox
    , addRight
    , justifyR
    , justifyL
    ) where

import           Imj.Prelude

import           Data.Either(partitionEithers)
import           Data.List(foldl')
import qualified Data.List as List
import           Data.String(IsString(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe(listToMaybe)
import           Data.Text(Text, pack)

import           Imj.Graphics.Color.Types

import           Imj.Data.Class.Quantifiable
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

maxLength :: [[a]] -> Int
maxLength = fromMaybe 0 . maximumMaybe . map List.length

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

showQuantities :: (Quantifiable a)
               => [a]
               -- ^ values
               -> [String]
showQuantities l' =
  showArrayN (listToMaybe header) body
 where
  l = avg : l'
  avg = average l'
  txts = map showQty l
  normalizedQuantities = normalize l
  graphical = map (\q -> replicate (round $ q*50) '|') normalizedQuantities
  (header, body) = List.splitAt 1
    $ map (\(i,g,t) -> [i, g, t])
    $ zip3
        ("Avg" : map show [1 :: Int ..])
        graphical
        txts

showQuantities' :: (Characters s, Quantifiable a)
                => a
                -- ^ Value to use for graphical representation when Left
                -> [Either s a]
                -- ^ Values
                -> [s]
showQuantities' leftValue l' =
  showArrayN (listToMaybe header) body
 where
  l = avg : l'
  avg = case partitionEithers l' of
    ([], valids) -> Right $ average valids
    (_:_,_) -> Left "N/A"
  txts = map (either id (fromString . showQty)) l -- TODO draw min in green
  normalizedQuantities = normalize $ map (either (const leftValue) id) l
  graphical = map (\q -> replicate (round $ q*50) '|') normalizedQuantities
  (header, body) = List.splitAt 1
    $ map (\(i,g,t) -> [i, g, t])
    $ zip3
        ("Avg" : map (fromString . show) [1 :: Int ..])
        graphical
        txts

-- | Shows times, underlying min and max times, and using a logarithmic scale
-- for the graphical representation.
showQuantities'' :: (Quantifiable a, Characters s)
                 => a
                 -- ^ Value used for Left (Timeout)
                 -> [Either Int a]
                 -- ^ Values (Left is a count of how many timeouts occured)
                 -> [s]
                 -- ^ Labels
                 -> s
                 -- ^ Title
                 -> [s]
showQuantities'' leftValue l labels title =
  showArrayN (Just [title]) body
 where
  txts = map (fromString . either (\n -> show n ++ " Timeouts") showQty) l
  normalizedQuantities = logarithmically 10 $ map (either (const leftValue) id) l

  worstVal = case partitionEithers l of
    ([],successes@(_:_)) -> Right $ maximum successes
    (timeouts@(_:_),_) -> Left $ maximum timeouts
    ([],[]) -> Left 0
  bestVal = case partitionEithers l of
    (_,successes@(_:_)) -> Right $ minimum successes
    (timeouts@(_:_),[]) -> Left $ minimum timeouts
    ([],[]) -> Left 0
  bestIndexes = List.elemIndices bestVal l
  worstIndexes = List.elemIndices worstVal l

  graphical = map (\q -> replicate (round $ q*50) '|') normalizedQuantities
  body =
    map
      (\(i,strs) ->
      let isBest = i `List.elem` bestIndexes
          isWorst = i `List.elem` worstIndexes
          (f, quality)
           | isBest && isWorst = (id, "")
           | isBest = (colorize $ onBlack green, "+")
           | isWorst = (colorize $ onBlack orange, "-")
           | otherwise = (id, "")
      in map f (strs ++ [quality]))
     $ zip [0..] $
      map (\(i,g,t) -> [i, g, t])
      $ zip3
          labels
          graphical
          txts

type Distribution a = Map a Int

{-# INLINABLE asDistribution #-}
asDistribution :: (Ord a)
               => [a] -> Distribution a
asDistribution = Map.fromListWith (+) . map (flip (,) 1)

{-# INLINABLE showDistribution #-}
showDistribution :: (Ord a, Show a, Enum a)
                 => Distribution a
                 -> [String]
showDistribution m =
  map
    (\(k,n) ->
      let s = show k
      in justifyL maxWidth s ++ " | " ++ replicate n '.')
    l
 where
  mayMin = Map.lookupMin m
  mayMax = Map.lookupMax m
  -- m' has no gap between min and max key (we add 0s if needed)
  m' = maybe m (\(min_,_) ->
      let (max_,_) = fromMaybe (error "logic") mayMax
      in foldl' (\ma k -> Map.insertWith (+) k 0 ma) m [min_..max_])
    mayMin
  l = Map.toAscList m'
  maxWidth = maxLength $ map (show . fst) l

{-# INLINABLE showListOrSingleton #-}
-- | If list is a singleton, show the element, else show the list.
showListOrSingleton :: Show a => [a] -> Text
showListOrSingleton [e] = pack $ show e
showListOrSingleton l   = pack $ show l
