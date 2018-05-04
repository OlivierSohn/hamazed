{-# LANGUAGE NoImplicitPrelude #-}

-- | This module defines colors of game elements.

module Imj.Game.Color
  ( -- * Config color
    configColors
  , configFgColor
  , darkConfigFgColor
  -- * Messages color
  , neutralMessageColor
  , neutralMessageColorFg
  , messageColor
  -- * Color themes centers
  , predefinedColor
  , predefinedColors
  , descPredefinedColors
  ) where


import           Imj.Prelude

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)

import           Imj.Graphics.Color
import           Imj.Game.Level

neutralMessageColorFg :: Color8 Foreground
neutralMessageColorFg = gray 10

neutralMessageColor :: LayeredColor
neutralMessageColor = onBlack neutralMessageColorFg

darkConfigFgColor, configFgColor :: Color8 Foreground
darkConfigFgColor = gray 4
configFgColor = gray 8

configColors :: LayeredColor
configColors = LayeredColor black configFgColor

messageColor :: LevelOutcome -> LayeredColor
messageColor Won      = onBlack $ rgb 4 3 1
messageColor (Lost _) = onBlack $ gray 6

predefinedColor :: String -> Maybe (Color8 Foreground)
predefinedColor = flip Map.lookup predefinedColors

descPredefinedColors :: String
descPredefinedColors =
  "{'" ++
  List.intercalate "','" (Map.keys predefinedColors) ++
  "'}"

predefinedColors :: Map String (Color8 Foreground)
predefinedColors = Map.fromList
  [ ("blue",     rgb 0 3 5)
  , ("violet",   rgb 1 0 3)
  , ("orange" ,  rgb 4 2 1)
  , ("olive"  ,  rgb 3 3 0)
  , ("reddish" , rgb 3 2 2)
  ]
