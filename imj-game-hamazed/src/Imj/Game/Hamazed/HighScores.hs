{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Game.Hamazed.HighScores
    ( getHighScores
    , HighScores(..)
    , HighScore(..)
    , prettyShowHighScores
    ) where

import           Imj.Prelude
import           Data.Text hiding (map)
import           Imj.Network
import           Imj.Game.Hamazed.Level

newtype HighScores = HighScores [HighScore]
  deriving(Generic,Show)
instance Binary HighScores
data HighScore = HighScore {
    _highScoreName :: [ClientName Approved]
  , _highScoreLevel :: {-# UNPACK #-} !LevelNumber
} deriving(Generic,Show)
instance Binary HighScore

getHighScores :: HighScores
getHighScores = HighScores
  [ HighScore ["me"] 2
  , HighScore ["me","you"] 5
  , HighScore ["her"] 4
  ]

prettyShowHighScores :: HighScores -> Text
prettyShowHighScores (HighScores h) = unlines $ map prettyShowHighScore h

prettyShowHighScore :: HighScore -> Text
prettyShowHighScore (HighScore players score) =
  unwords $ map (pack . show) players ++ [pack $ show score]
