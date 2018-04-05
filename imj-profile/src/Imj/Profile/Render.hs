{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Profile.Render
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
inverseMap :: (Quantifiable a) => IntMap a -> Map a IntSet
inverseMap = Map.fromListWith ISet.union . map (fmap ISet.singleton . swap) . IMap.toList

-- | Shows times, underlying min and max times, and using a logarithmic scale
-- for the graphical representation.
showTestResults :: Characters s
                 => [TestResult a]
                 -> [s]
                 -- ^ Labels
                 -> s
                 -- ^ Title
                 -> [s]
showTestResults l labels title =
  showArrayN (Just [title, "", fromString $ List.unwords ["Best:", showTime bestVal]]) body
 where
  lMap = IMap.fromDistinctAscList $ zip [0..] l

  txts = map
    (fromString . (\case
      SomeTimeout n dt -> unwords [show n, showQty dt, "Timeout"]
      Finished dt -> showQty $ mean dt
      Cancelled -> "Cancelled" ))
      l

  normalizedQuantities = IMap.union validOrTimeoutNormalizedQuantities cancelledNormalizedQuantities

  validOrTimeoutNormalizedQuantities =
    IMap.fromDistinctAscList $
    zip (IMap.keys validOrTimeout) $
    map Just $
    logarithmically 10 $ IMap.elems validOrTimeout

  cancelledNormalizedQuantities =
    IMap.map (const Nothing) cancelled

  (cancelled, validOrTimeout) = IMap.mapMaybe id <$> IMap.partition isNothing allQuantities

  allQuantities = IMap.map (\case
    Cancelled -> Nothing
    SomeTimeout _ timeout -> Just timeout
    Finished dt -> Just $ mean dt)
    lMap

  (timeoutCounts', validDurations') = IMap.mapEither (\case
    Cancelled -> error "logic" -- has been filtered by mapMaybe
    SomeTimeout n dt -> Left $ fromIntegral n .* dt
    Finished dt -> Right $ mean dt) $
    IMap.filter (\case Cancelled -> False; _ -> True) lMap

  timeoutCounts = inverseMap timeoutCounts'
  validDurations = inverseMap validDurations'

  (_, worstIndexes) =
    fromMaybe
      (fromMaybe (zeroDuration, ISet.empty) $ Map.lookupMax validDurations)
      $ Map.lookupMax timeoutCounts

  (bestVal, bestIndexes) =
    fromMaybe
      (fromMaybe (zeroDuration, ISet.empty) $ Map.lookupMin timeoutCounts)
      $ Map.lookupMin validDurations

  barSize = 50
  graphical = map
    (maybe
      (replicate barSize '.')
      (\q -> replicate (round $ q* fromIntegral barSize) '|')) $
    IMap.elems normalizedQuantities
  body =
    map
      (\(i,strs) ->
      let isBest = i `ISet.member` bestIndexes
          isWorst = i `ISet.member` worstIndexes
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

maxLength :: [[a]] -> Int
maxLength = fromMaybe 0 . maximumMaybe . map List.length
