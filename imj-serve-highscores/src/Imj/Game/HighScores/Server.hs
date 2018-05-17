{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Imj.Game.HighScores.Server
  ( serveHighScores
  ) where

import Prelude ()
import Prelude.Compat

-- TODO add features of http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html

{-
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
-}

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

{-
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
-}

import Imj.Game.HighScores
import Imj.Game.HighScores.API
import Imj.Game.HighScores.Server.Args
import Imj.Network

getHighScoresHandler :: Handler HighScores
getHighScoresHandler = return mkEmptyHighScores

insertHighScoreHandler :: HighScore -> Handler HighScores
insertHighScoreHandler _ = return mkEmptyHighScores

healthHandler :: Int -> Handler Int
healthHandler i = return $ i + 1

serveHighScoresApp :: Application
serveHighScoresApp =
  serve highScoresAPI
       $ healthHandler
    :<|> getHighScoresHandler
    :<|> insertHighScoreHandler

serveHighScores :: IO ()
serveHighScores = withArgs parserSrvPort $ \portArg -> do
  p <- maybe (return defaultPort) (fmap unServerPort . getServerPort) portArg
  run p serveHighScoresApp
