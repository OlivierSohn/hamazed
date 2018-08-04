{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Color
  (
    -- * 8-bits colors
    {- | There are
    <https://en.wikipedia.org/wiki/ANSI_escape_code#Colors several types of colors we can use to draw in a terminal>.
    Here, we support 8-bit colors.

    8-bit colors have 6*6*6 = 216 rgb colors, 24 grays and are represented by
    'Color8'. You can create a 'Color8' ny using 'rgb' or 'gray'.

    It is possible to /interpolate/ between two colors in RGB space using the
    'DiscreteInterpolation' instance of 'Color8'.

    We also have 'LayeredColor' because when drawing in the terminal, we can
    change both the 'Background' and the 'Foreground' color.
    -}
    Color8 -- constructor is intentionaly not exposed.
    -- ** Create a single color
  , rgb
  , gray
  , mix
  , dim
  -- ** Get / modify hue
  , hue
  , countHuesOfSameIntensity
  , rotateHue
    -- ** Create a LayeredColor
  , LayeredColor(..)
  , Background
  , Foreground
  , onBlack
  , whiteOnBlack
    -- ** Primary colors
  , red, green, blue
    -- ** Secondary colors
  , magenta, cyan, yellow
    -- ** Tertiary colors
  , azure, violet, rose, orange, chartreuse, springGreen
    -- ** Quaternary colors
  , lime
    -- ** Predefined colors
  , white, black
  ) where

import           Imj.Graphics.Color.Types
import           Imj.Graphics.Color.Hue
