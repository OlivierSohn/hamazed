{-# LANGUAGE NoImplicitPrelude #-}

-- | This module defines colors of game elements.

module Imj.Game.ColorTheme.Class
  ( ColorTheme(..)
  ) where

import           Imj.Prelude
import           Imj.Graphics.Color

class (Show p) => ColorTheme p where
  mkColorTheme :: Color8 Foreground -> p

instance ColorTheme () where
  mkColorTheme _ = ()
