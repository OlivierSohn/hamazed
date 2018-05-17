{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Imj.Game.HighScores.API
  ( HighScoresAPI
  , highScoresAPI
  ) where

import Imj.Prelude

import Servant

import Imj.Game.HighScores

type HighScoresAPI =
        "health"     :> ReqBody '[JSON] Int :> Post '[JSON] Int
   :<|> "highscores" :> Get     '[JSON] HighScores
   :<|> "highscores" :> ReqBody '[JSON] HighScore :> Post '[JSON] HighScores

highScoresAPI :: Proxy HighScoresAPI
highScoresAPI = Proxy
