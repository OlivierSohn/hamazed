{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Network.ClientId
      ( mkClientColorFromCenter
      , checkName
      ) where

import           Imj.Prelude

import           Data.Char (isPunctuation, isSpace)
import           Data.Text(Text)

import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Graphics.Color.Types
import           Imj.Server.Internal.Types

import           Imj.Graphics.Color

checkName :: String -> Either Text ()
checkName name
  | any ($ name) [ null, any isPunctuation, any isSpace] =
      Left "Name cannot contain punctuation or whitespace, and cannot be empty"
  | otherwise =
      Right ()

mkClientColorFromCenter :: ClientId -> Color8 Foreground -> Color8 Foreground
mkClientColorFromCenter (ClientId i) ref =
  let nColors = countHuesOfSameIntensity ref
      -- we want the following mapping:
      -- 0 -> 0
      -- 1 -> 1
      -- 2 -> -1
      -- 3 -> 2
      -- 4 -> -2
      -- ...
      dist = quot (succ i) 2
      n' = fromIntegral dist `mod` nColors
      n = if odd i then n' else -n'
  in rotateHue (fromIntegral n / fromIntegral nColors) ref
