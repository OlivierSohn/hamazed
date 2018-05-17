{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Hamazed.HighScores
    ( getHighScores
    , HighScores(..)
    , HighScore(..)
    ) where

import Imj.Network
import Imj.Game.Hamazed.Level

newtype HighScores = HighScores [HighScore]

data HighScore = HighScore {
    _highScoreName :: {-# UNPACK #-} !(ClientName Approved)
  , _highScoreLevel :: {-# UNPACK #-} !LevelNumber
}

getHighScores :: HighScores
getHighScores = HighScores
  [ HighScore "me" 2
  , HighScore "you" 5
  , HighScore "her" 4
  ]
