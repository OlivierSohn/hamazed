{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Imj.Game.ArgParse
      ( srvNameArg
      ) where

import           Imj.Prelude

import           Data.Char(toLower)
import           Options.Applicative(str, ReadM, readerError)

import           Imj.ServerView.Types

srvNameArg :: ReadM ServerName
srvNameArg =
  str >>= \s -> case map toLower s of
    [] -> readerError "Encountered an empty servername. Accepted names are ip address or domain name."
    name -> return $ ServerName name
