{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Profile.Render.Characters
    ( -- * distribution utilities
      asDistribution
    , Distribution
    -- * Render utilities
    , showDistribution
    , showQuantities
    , showQuantities'
    , showTestResults
    ) where

import           Imj.Prelude

import           Data.Either(partitionEithers)
import           Data.List(foldl')
import qualified Data.List as List
import           Data.String(IsString(..))
import           Data.Tuple(swap)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IMap
import           Data.IntSet(IntSet)
import qualified Data.IntSet as ISet
import           Data.Maybe(listToMaybe)
import           Numeric(showFFloat)

import           Imj.Graphics.Color.Types
import           Imj.Graphics.Class.Words

import           Imj.Data.Class.Quantifiable
import           Imj.Graphics.Text.Render
import           Imj.Profile.Result
import           Imj.Util
import           Imj.Timing

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

{-# INLINABLE inverseMap #-}
inverseMap :: (Ord a) => IntMap a -> Map a IntSet
inverseMap = Map.fromListWith ISet.union . map (fmap ISet.singleton . swap) . IMap.toList

-- | Shows times, underlying min and max times, and using a logarithmic scale
-- for the graphical representation.
showTestResults :: Characters s
                 => Maybe (Time Duration System)
                 -- ^ Timeout value
                 -> [(s, TestDurations k a)]
                 -- ^ fst is labels
                 -> s
                 -- ^ Title
                 -> [(Maybe (TestDurations k a), [s])]
                 -- ^ [s] is a list of lines corresponding to maybe one test result
showTestResults timeoutValue l title =
  map ((,) Nothing) (IMap.elems footerMap) ++
  zipWith
    (\lines res -> (Just res,lines))
    (IMap.elems bodyMap)
    (IMap.elems resultsMap) ++
  map ((,) Nothing) (IMap.elems headerMap)
 where
  (footerMap, headerMap) = IMap.split 0 $ IMap.withoutKeys indexed resultKeys
  bodyMap = IMap.restrictKeys indexed resultKeys

  indexed = indexedShowArrayN
    (Just [ title
          , fromString $ List.unwords ["Best mean:", bestValStr]
          , "Mean"
          , "Dispersion"
          , ""]) body

  lMap = IMap.fromDistinctAscList $ zip [0..] l

  normalizedQuantities = IMap.union validOrTimeoutNormalizedQuantities cancelledNormalizedQuantities

  validOrTimeoutNormalizedQuantities =
    IMap.fromDistinctAscList $
    zip (IMap.keys validOrTimeout) $
    map Just $
    logarithmically 10 $ map (\case
      NTimeouts _ -> fromMaybe (error "Please provide a value") timeoutValue
      FinishedAverage dt _ -> dt
      NoResult -> error "logic") $ IMap.elems validOrTimeout

  cancelledNormalizedQuantities =
    IMap.map (const Nothing) cancelled

  (cancelled, validOrTimeout) = IMap.partition (\case NoResult -> True; _ -> False) summary

  resultsMap = IMap.map snd lMap
  resultKeys = IMap.keysSet resultsMap

  summary = IMap.map summarize resultsMap

  invMap = inverseMap summary

  worstIndexes =
    maybe ISet.empty snd $ Map.lookupMax invMap

  (bestVal, bestIndexes) =
    fromMaybe (NoResult, ISet.empty) $ Map.lookupMin invMap
  bestValStr = case bestVal of
    NoResult -> "?"
    NTimeouts n -> unwords [show n, "Timeout(s)", showTime $ fromMaybe (error "please provide a value") timeoutValue]
    FinishedAverage x _ -> showTime  x

  barSize = 25 :: Int

  graphical = IMap.map
    (maybe "?" (\q -> replicate (round $ q* fromIntegral barSize) '|'))
    normalizedQuantities

  body = IMap.fromDistinctAscList $
    map (\(s,g,(i,su)) ->
        let t = fromString $ case su of
                  NTimeouts n -> unwords [show n, "Timeout(s)"]
                  FinishedAverage dt _ -> showQty dt
                  NoResult -> "?"
            v = fromString $ case su of
                  NTimeouts _ -> "."
                  FinishedAverage _ dispersion -> showFFloat (Just 0) (100 * realToFrac dispersion :: Float) " %"
                  NoResult -> "?"
            isBest = i `ISet.member` bestIndexes
            isWorst = i `ISet.member` worstIndexes
            (style, q)
             | isBest && isWorst = (id, "")
             | isBest = (colorize $ onBlack green, "+")
             | isWorst = (colorize $ onBlack orange, "-")
             | otherwise = (id, "")
        in (i,map style [s, g, t, v, q]))
      $ zip3
          (map fst l)
          (IMap.elems graphical)
          $ IMap.assocs summary

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

maxLength :: [[a]] -> Int
maxLength = fromMaybe 0 . maximumMaybe . map List.length
