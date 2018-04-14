{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.World.Space.Strategies
    ( OptimalStrategies(..)
    , OptimalStrategy(..)
    , encodeOptimalStrategiesFile
    , decodeOptimalStrategiesFileOrFail
    , smallWorldCharacteristicsDistance
    ) where

import           Imj.Prelude

import           Prelude(FilePath, putStrLn)
import           Data.Binary
import           Data.Map.Strict(Map)
import           System.Directory(doesFileExist)

import           Imj.Game.Hamazed.World.Space.Types
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

data OptimalStrategy = OptimalStrategy {
    _optinalStrategy :: !(Maybe MatrixVariantsSpec)
  , averageDuration :: !(Time Duration System)
} deriving(Generic)
instance Binary OptimalStrategy
instance Ord OptimalStrategy where
  compare a b = compare (averageDuration a) (averageDuration b)
instance Eq OptimalStrategy where
  a == b = averageDuration a == averageDuration b

smallWorldCharacteristicsDistance :: SmallWorldCharacteristics ->SmallWorldCharacteristics -> Float
smallWorldCharacteristicsDistance (SWCharacteristics sz cc p) (SWCharacteristics sz' cc' p') =
  -- These changes have the same impact on distance:
  --   doubled size
  --   proba 0.6 -> 0.7
  --   2 cc -> 3 cc
  --   4 cc -> 5 cc
  dSize + 10 * abs (p-p') + fromIntegral (dCC (min cc cc') (max cc cc'))
 where
   -- c1 <= c2
   dCC c1 c2
    | c1 == c2 = 0
    | c1 == 1 = (c2 - c1) * 10
    | otherwise = c2 - c1

   dSize
    | a == a' = 0
    | a' == 0 = 1000000
    | ratio > 1 = ratio - 1
    | otherwise = (1 / ratio) - 1
    where
      a = area sz
      a' = area sz'
      ratio = fromIntegral a / fromIntegral a'

optimalStrategiesFile :: FilePath
optimalStrategiesFile = "optstrat.bin"

encodeOptimalStrategiesFile :: OptimalStrategies -> IO ()
encodeOptimalStrategiesFile s = do
  encodeFile optimalStrategiesFile s
  putStrLn $ "Wrote optimal strategies file:" ++ show optimalStrategiesFile

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
