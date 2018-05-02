{-# LANGUAGE NoImplicitPrelude #-}

-- | This module defines colors of game elements.

module Imj.Game.Color (
    configColors
  , configFgColor
  , darkConfigFgColor
  , predefinedColor
  , predefinedColors
  , descPredefinedColors
  -- * Reexports
  , module Imj.Graphics.Color
  ) where


import           Imj.Prelude
import           Imj.Graphics.Color
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)

darkConfigFgColor, configFgColor :: Color8 Foreground
darkConfigFgColor = gray 4
configFgColor = gray 8

configColors :: LayeredColor
configColors = LayeredColor black configFgColor

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
