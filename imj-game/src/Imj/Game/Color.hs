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
  ) where


import           Imj.Prelude

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
