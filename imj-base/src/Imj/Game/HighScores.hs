{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Game.HighScores
    ( HighScores(..)
    , HighScore(..)
    , mkEmptyHighScores
    , insertScore
    , prettyShowHighScores
    ) where

import           Imj.Prelude
import           Data.Aeson(ToJSON(..), FromJSON(..))
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.Text hiding (map, concatMap)

import           Imj.Network
import           Imj.Game.Level

newtype HighScores = HighScores (Map LevelNumber (Set (Set (ClientName Approved))))
  deriving(Generic,Show)
instance Binary HighScores
instance NFData HighScores
instance ToJSON HighScores
instance FromJSON HighScores

data HighScore = HighScore !LevelNumber !(Set (ClientName Approved))
  deriving(Generic,Show)
instance Binary HighScore
instance NFData HighScore
instance ToJSON HighScore
instance FromJSON HighScore

mkEmptyHighScores :: HighScores
mkEmptyHighScores = HighScores mempty

insertScore :: LevelNumber -> Set (ClientName Approved) -> HighScores -> HighScores
insertScore n players (HighScores hs) =
  HighScores $ Map.insertWith Set.union n (Set.singleton players) hs

prettyShowHighScores :: HighScores -> Text
prettyShowHighScores (HighScores h) =
  unlines $ map (uncurry prettyShowHighScore) $ Map.toDescList h

prettyShowHighScore :: LevelNumber -> Set (Set (ClientName Approved)) -> Text
prettyShowHighScore score players =
  unwords $ map (pack . show . unClientName) (concatMap Set.toList $ Set.toList players) ++ [pack $ show score]
