{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Imj.Game.HighScores.Client
  (-- * Monitoring, unidling
    unidleHighScoresServer
  , highScoresServerHealth
  -- * REST api
  , getHighScores
  , addHighScore
  ) where

import Imj.Prelude

import Servant.API
import Servant.Client

import Imj.Game.HighScores
import Imj.Game.HighScores.API

-- | Returns the successor of the value passed.
--
-- This function can be used to monitor the server.
highScoresServerHealth :: Int -> ClientM Int

-- | Returns the high scores of the Hamazed game.
getHighScores :: ClientM HighScores

-- | Adds a high score to the Hamazed game.
addHighScore :: HighScore -> ClientM HighScores

highScoresServerHealth
  :<|> getHighScores
  :<|> addHighScore
  = client highScoresAPI

-- | Web apps hosted on Heroku using the free plan become idle after 30 minutes
-- of inactivity, and then need 30 seconds to unidle.
--
-- This function unidles the server.
unidleHighScoresServer :: ClientM ()
unidleHighScoresServer = void $ highScoresServerHealth 0
