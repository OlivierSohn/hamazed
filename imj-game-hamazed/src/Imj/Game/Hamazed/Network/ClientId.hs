{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Network.ClientId
      ( mkClientColorFromCenter
      ) where

import           Imj.Prelude

import           Imj.ClientView.Types
import           Imj.Graphics.Color.Types

import           Imj.Graphics.Color

-- | This function assumes that ClientId's start at 0 and are ascending.
mkClientColorFromCenter :: ClientId -> Color8 Foreground -> Color8 Foreground
mkClientColorFromCenter i ref =
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
