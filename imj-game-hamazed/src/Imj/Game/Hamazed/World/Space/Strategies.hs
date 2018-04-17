{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.World.Space.Strategies
    ( OptimalStrategies(..)
    , OptimalStrategy(..)
    , prettyShowOptimalStrategies
    , encodeOptimalStrategiesFile
    , closestOptimalStrategy
    , ClosestOptimalStrategy(..)
    , smallWorldCharacteristicsDistance
    ) where

import           Imj.Prelude

import           Prelude(putStrLn)
import           Data.FileEmbed(embedFile)
import           Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.String(IsString(..))

import           Imj.Data.AlmostFloat
import           Imj.Game.Hamazed.World.Space.Strategies.Internal
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Class.Words hiding(intercalate)
import           Imj.Graphics.Text.Render
import           Imj.Timing

-- NOTE we could record also non optimal strategies, because when we interpolate, maybe the points of the cube do not agree:
-- p1 says "strategy1 is optimal", "strategy2 is good"
-- p2 says "strategy 1 is the worst", "strategy 2 is good", "strategy 3 is optimal"
--
-- if we want to take a chance, we can try strategy 1, and hope that it will behave more like p1 than p2
-- or we can play safe and pick the one they agree on for being good.
--
-- For now, we'll just record the optimal strategies, assuming that the space of worlds is sufficiently sampled
-- to say that such situations won't occur.
-- ... of course, the safest approach is to sample exactly the world characteristics that we will use in the game.
newtype OptimalStrategies = OptimalStrategies (Map SmallWorldCharacteristics OptimalStrategy)
  deriving(Generic)
instance Binary OptimalStrategies
instance Show OptimalStrategies where
  show = unlines . prettyShowOptimalStrategies

data OptimalStrategy = OptimalStrategy {
    _optimalStrategy :: !(Maybe MatrixVariantsSpec)
  , averageDuration :: !(Time Duration System)
} deriving(Generic)
instance Binary OptimalStrategy
instance Ord OptimalStrategy where
  compare a b = compare (averageDuration a) (averageDuration b)
instance Eq OptimalStrategy where
  a == b = averageDuration a == averageDuration b


data StratDist = StratDist {-# UNPACK #-} !OptimalStrategy {-# UNPACK #-} !Float {-# UNPACK #-} !SmallWorldCharacteristics

data ClosestOptimalStrategy = ClosestOptimalStrategy {
    _closestWorld :: !(Maybe SmallWorldCharacteristics)
    -- ^ Note that the bigger the distance is, the less accurate the duration estimation will be.
  , _closestEstimatedDuration :: !(Maybe (Time Duration System))
  , _closestMatrixVariant :: !(Maybe MatrixVariantsSpec)
}

closestOptimalStrategy :: SmallWorldCharacteristics -> Either ClosestOptimalStrategy OptimalStrategy
closestOptimalStrategy world = case embeddedOptimalStrategies of
  (OptimalStrategies m) ->
    let closest =
          Map.foldlWithKey'
            (\prev charac strategy ->
              let thisDist = smallWorldCharacteristicsDistance charac world
                  this = Just $ StratDist strategy thisDist charac
              in maybe
                this
                (\(StratDist _ prevDist _) ->
                  if thisDist < prevDist
                    then
                      this
                    else
                      prev)
                prev)
            Nothing
            m
        closestOptimal =
          maybe
            (ClosestOptimalStrategy Nothing Nothing defaultStrategy)
            (\(StratDist (OptimalStrategy s dt) _ charac) ->
              ClosestOptimalStrategy (Just charac) (Just dt) s)
            closest
    in maybe (Left closestOptimal) Right $ Map.lookup world m
 where
  defaultStrategy = Nothing

smallWorldCharacteristicsDistance :: SmallWorldCharacteristics -> SmallWorldCharacteristics -> Float
smallWorldCharacteristicsDistance (SWCharacteristics sz cc p) (SWCharacteristics sz' cc' p') =
  -- These changes have the same impact on distance:
  --   doubled size
  --   proba 0.6 -> 0.7
  --   2 cc -> 3 cc
  --   4 cc -> 5 cc
  doubleSize + 10 * abs (p-p') + fromIntegral (dCC (min cc cc') (max cc cc'))
 where
   -- c1 <= c2
   dCC c1 c2
    | c1 == c2 = 0
    | c1 == 1 = (c2 - c1) * 10
    | otherwise = c2 - c1

   doubleSize -- 1 when size doubles
    | a == a' = 0
    | a' == 0 = 1000000
    | ratio > 1 = ratio - 1
    | otherwise = (1 / ratio) - 1
    where
      a = area sz
      a' = area sz'
      ratio = fromIntegral a / fromIntegral a'

encodeOptimalStrategiesFile :: OptimalStrategies -> IO ()
encodeOptimalStrategiesFile s = do
  encodeFile path s
  putStrLn $ "Wrote optimal strategies file:" ++ show path
 where
  path = "./imj-game-hamazed/" <> optimalStrategiesFile

embeddedOptimalStrategies :: OptimalStrategies
embeddedOptimalStrategies =
  either
    (\(_,offset,str) -> error $ "The embedded file optstrat.bin is corrupt:" ++ show (offset,str))
    (\(_,offset,res) ->
      if fromIntegral len == offset
        then
          res
        else
          error $ "Not all content has been used :" ++ show (len,offset) ) $
    decodeOrFail $ L.fromStrict content
 where
   content = $(embedFile optimalStrategiesFile)
   len = B.length content

{-
decodeOptimalStrategiesFileOrFail :: IO OptimalStrategies
decodeOptimalStrategiesFileOrFail =
  doesFileExist optimalStrategiesFile >>= \case
    True -> decodeFileOrFail optimalStrategiesFile >>= either
      (\e -> do
        putStrLn $ "File " ++ optimalStrategiesFile ++ " seems corrupt: " ++ show e
        return fallback)
      return
    False -> do
      putStrLn $ "File " ++ optimalStrategiesFile ++ " not found, using default strategy."
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
    $ Map.toList $ splitKeys (\(SWCharacteristics sz cc proba) -> (cc, SizeProba sz $ almost proba)) m
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
