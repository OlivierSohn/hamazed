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
      , maxMIDIJitterArg
      ) where

import           Imj.Prelude

import           Data.Char(toLower)
import           Data.Int(Int64)
import           Options.Applicative(str, ReadM, readerError)
import           Text.Read(readMaybe)

import           Imj.Audio.Midi
import           Imj.ServerView.Types

srvNameArg :: ReadM ServerName
srvNameArg =
  str >>= \s -> case map toLower s of
    [] -> readerError "Encountered an empty servername. Accepted names are ip address or domain name."
    name -> return $ ServerName name

maxMIDIJitterArg :: ReadM MaxMIDIJitter
maxMIDIJitterArg =
  str >>= maybe
    (readerError "Encountered an unreadable max MIDI jitter. This should be a positive number")
    (\(i :: Int64) ->
      if i < 0
        then
          readerError "Encountered a negative max MIDI jitter."
        else
          return $ fromIntegral i)
    . readMaybe
