{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.HighScores.Server.Args
    ( defaultPort
    , withArgs
    , parserSrvPort
    ) where

import           Imj.Prelude
import           Prelude.Compat

import           Options.Applicative
                  (ParserHelp(..), Parser, fullDesc, info, header, execParserPure, prefs, helper
                  , showHelpOnError, (<*>), help, short, long, option, optional)
import           Options.Applicative.Extra(handleParseResult, overFailure)
import qualified Options.Applicative.Help as Appli (red)
import           System.Environment(getArgs, getProgName)

import           Imj.Network

parserSrvPort :: Parser (Maybe ArgServerPort)
parserSrvPort =
  optional
    (option srvPortArg
       (  long "serverPort"
       <> short 'p'
       <> help (
       "The listening port. Can be a number or an environment variable. " ++
       "Defaults to " ++ show (toInteger defaultPort) ++ ".")
       ))

withArgs :: Parser x -> (x -> IO a) -> IO a
withArgs parser app = do
  progn <- getProgName
  join $ execParserPure (prefs showHelpOnError) (parserInfo progn) <$> getArgs >>=
    handleParseResult . overFailure (\ph -> ph {helpError = fmap Appli.red $ helpError ph})
 where
  parserInfo n =
    info (helper <*> (app <$> parser))
    (  fullDesc
    <> header (
       n ++ " serves highscores."
      ))

defaultPort :: Int
defaultPort = 8081
