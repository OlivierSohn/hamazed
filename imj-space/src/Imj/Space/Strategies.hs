{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Space.Strategies
    ( OptimalStrategies(..)
    , OptimalStrategy(..)
    , StrategyTag(..)
    , prettyShowOptimalStrategies
    , encodeOptimalStrategiesFile
    , lookupOptimalStrategy
    , readOptimalStrategies
    ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS
import           Language.Haskell.TH.Syntax(lift)

import           Imj.Prelude
import qualified Prelude as Unsafe(last)
import           Prelude(putStrLn)
import           Data.Binary
import qualified Data.ByteString.Lazy as BL
import           Data.List
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.String(IsString(..))

import           Imj.Data.AlmostFloat
import           Imj.Graphics.Class.Words hiding(intercalate, length)
import           Imj.Graphics.Text.Render
import           Imj.Space.Types
import           Imj.Timing
import           Imj.Util

-- NOTE we could record also non optimal strategies, because when we interpolate, maybe the points of the cube do not agree:
-- p1 says "strategy1 is optimal", "strategy2 is good"
-- p2 says "strategy 1 is the worst", "strategy 2 is good", "strategy 3 is optimal"
--
-- if we want to take a chance, we can try strategy 1, and hope that it will behave more like p1 than p2
-- or we can play safe and pick the one they agree on for being good.
newtype OptimalStrategies = OptimalStrategies (Map (SmallWorldCharacteristics Program) OptimalStrategy)
  deriving(Generic)
instance Lift OptimalStrategies where
  lift (OptimalStrategies m) =
     [| OptimalStrategies (Map.fromList $(lift (Map.toList m))) |]
instance Binary OptimalStrategies
instance Show OptimalStrategies where
  show = unlines . prettyShowOptimalStrategies

data OptimalStrategy = OptimalStrategy {
    _optimalStrategy :: !(Maybe MatrixVariantsSpec)
  , averageDuration :: !(Time Duration System)
} deriving(Generic, Lift)
instance Binary OptimalStrategy
instance Ord OptimalStrategy where
  compare a b = compare (averageDuration a) (averageDuration b)
instance Eq OptimalStrategy where
  a == b = averageDuration a == averageDuration b

data StrategyTag =
    Refined
  | Unrefined ![(Maybe MatrixVariantsSpec)]
  -- with a 'List' of untested strategies.
  deriving(Generic)
instance Binary StrategyTag
instance NFData StrategyTag
instance Show StrategyTag where
  show Refined = "Refined"
  show (Unrefined _) = "Unrefined" -- for html report

-- | Returns 'Right' when the 'OptimalStrategies' contain at least one
-- 'SmallWorldCharacteristics' 'Program' that matches exactly the 'Size' and 'ComponentCount'
-- of the 'SmallWorldCharacteristics' 'User' passed as parameter, and can be built within budget.
--
-- Note that the 'AlmostFloat' probability of 'SmallWorldCharacteristics' 'User'
-- will not be exactly matched, instead, it will be adapted by mapping [0,1] to [l,h],
-- where l == min probability where duration < budget
--       h == max probability where duration < budget
--
-- The returned 'SmallWorldCharacteristics Program' will be the one that has a probability
-- closest to the mapped probability.
--
-- 'OptimalStrategies' should contain all combinations of 'Size' and 'ComponentCount' values
-- used in the game. Typically, the 'OptimalStrategies' will be precalculated :
-- see imj-profile / 'mkOptimalStrategies' (which currently precalculates for values of imj-game-hamazed).
lookupOptimalStrategy :: SmallWorldCharacteristics User
                      -> Time Duration System
                      -- ^ The budget duration to create the world.
                      -> OptimalStrategies
                      -> Either () (SmallWorldCharacteristics Program, OptimalStrategy)
lookupOptimalStrategy (SWCharacteristics sz nComps proba) maxDuration(OptimalStrategies m)  =
  let ok = Map.mapKeysMonotonic userWallProbability $
           Map.filterWithKey
            (\(SWCharacteristics sz' nComps' _) (OptimalStrategy _ dt) ->
              dt < maxDuration && sz' == sz && nComps' == nComps)
            m
  in case Map.assocs ok of
      [] -> Left ()
      l@((minProba,_):_) ->
        let (maxProba,_) = Unsafe.last l
            len = length l
            (_,strategy) = l !! round (proba * (fromIntegral $ len - 1))
            adjustedProba = fromMaybe (error "logic") $ mapRange 0 1 minProba maxProba proba
        in Right $ (SWCharacteristics sz nComps adjustedProba, strategy)


encodeOptimalStrategiesFile :: FilePath
                            -> OptimalStrategies
                            -> IO ()
encodeOptimalStrategiesFile path s = do
  encodeFile path s
  putStrLn $ "Wrote optimal strategies file:" ++ show path

readOptimalStrategies :: FilePath -> TH.ExpQ
readOptimalStrategies path = do
  bl <- TH.runIO $ BL.readFile path
  let len = BL.length bl
  either
    (\(_,offset,str) -> fail $ "The file '" ++ path ++ "' is corrupt:" ++ show (offset,str))
    (\(_,offset,res :: OptimalStrategies) ->
      if fromIntegral len == offset
        then
          THS.lift res
        else
          fail $ "Not all content has been used :" ++ show (len,offset) ) $
    (decodeOrFail bl)


{-
decodeOptimalStrategiesFileOrFail :: IO OptimalStrategies
decodeOptimalStrategiesFileOrFail =
  doesFileExist optimalStrategiesFilepath >>= \case
    True -> decodeFileOrFail optimalStrategiesFilepath >>= either
      (\e -> do
        putStrLn $ "File " ++ optimalStrategiesFilepath ++ " seems corrupt: " ++ show e
        return fallback)
      return
    False -> do
      putStrLn $ "File " ++ optimalStrategiesFilepath ++ " not found, using default strategy."
      return fallback
 where
  fallback = OptimalStrategies mempty
-}

data SizeProba = SizeProba {
    _spSize :: {-# UNPACK #-} !Size
  , _spProba :: {-# UNPACK #-} !AlmostFloat
} deriving(Eq, Ord)

prettyShowOptimalStrategies :: Characters s => OptimalStrategies -> [s]
prettyShowOptimalStrategies (OptimalStrategies m) =
  intercalate [""] $ map (\(cc,optimalStrategies) -> fromString (show cc) : showBySizeProba optimalStrategies)
    $ Map.toList $ splitKeys (\(SWCharacteristics sz cc proba) -> (cc, SizeProba sz proba)) m
 where
  showBySizeProba m' = map fromString $ showArrayN
    (Just $ "" : map show allProbas) $
    map showFullLine $ sortOn (area . fst) fullLines -- order by area
   where
    sparseLines :: [(Size, Map AlmostFloat OptimalStrategy)]
    sparseLines = Map.assocs $ splitKeys (\(SizeProba sz p) -> (sz, p)) m'

    allProbas = Set.toList $ Set.unions $ map (Map.keysSet . snd) sparseLines

    common = Map.fromDistinctAscList $ zip allProbas $ repeat Nothing

    fullLines :: [(Size, [Maybe OptimalStrategy])]
    fullLines = map (fmap $ Map.elems . safeMerge (<|>) common . Map.map Just) sparseLines

    showFullLine (Size (Length a) (Length b), l) = show (a,b) : map (maybe "" (showTime . averageDuration)) l


splitKeys :: (Ord k, Ord k1, Ord k2)
          => (k -> (k1, k2))
          -- ^ is expected to be injective
          -> Map k v
          -> Map k1 (Map k2 v)
splitKeys convertKey =
  Map.map (Map.mapKeysWith (error "logic") (snd . convertKey))
  . Map.mapKeysWith Map.union (fst . convertKey) . Map.mapWithKey Map.singleton
