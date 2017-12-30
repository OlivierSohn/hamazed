{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Color
  (
    -- * 8-bits colors
    {- | There are
    <https://en.wikipedia.org/wiki/ANSI_escape_code#Colors several types of colors we can use to draw in a terminal>.

    The terminal I was developping with didn't support 24-bit colors, hence I
    used 8-bit colors.

    8-bit colors have 6*6*6 = 216 rgb colors and 24 grays. They are represented by
    'Color8', which also provides a 'DiscreteDistance' instance : using a 3D
    Bresenham line algorithm, we can smoothly interpolate, in RGB space, between
    two rgb colors or between two grays.

    When drawing in the terminal, we can change both the 'Background' and the
    'Foreground' colors. 'LayeredColor' represents this.

    The constructor of 'Color8' is not exported, however if you want to go back to the rgb
    or gray values, use 'color8CodeToXterm256' which will give you a 'Xterm256Color'.
    For the inverse operation, use 'xterm256ColorToCode'.
    -}
    Color8
    -- ** Create a single color
    {- | 'rgb' creates a red, green, blue color with 6x6x6 possible combinations.
    To see how the color is mapped to a color of an sRGB colorspace, see 'xtermMapRGB8bitComponent'.

    'gray' creates a gray terminal color with 24 possible shades.
    To see how the color is mapped to a color of an sRGB colorspace, see 'xtermMapGray8bitComponent'. -}
  , rgb
  , gray
    -- ** Introspect a single color
  , color8CodeToXterm256
  , xterm256ColorToCode
  , Xterm256Color(..)
    -- ** Create a LayeredColor
  , LayeredColor(..)
  , Background
  , Foreground
  , onBlack
  , whiteOnBlack
    -- ** Predefined colors
  , white, black, red, green, magenta, cyan, yellow, blue
    -- ** SGR codes
  , color8BgSGRToCode
  , color8FgSGRToCode
    -- ** Interpolation
    {-| We can interpolate between two 'Color8', provided that
    they were both created either using 'rgb' or both created using 'gray'.-}
  , bresenhamColor8
  , bresenhamColor8Length
    -- ** XTerm color mapping
  , xtermMapGray8bitComponent
  , xtermMapRGB8bitComponent
    -- * Reexports
  , DiscreteDistance
  , RGB(..)
  ) where

import           Imj.Prelude

import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Graphics.Color.Types

-- | How
-- <https://jonasjacek.github.io/colors/ xterm interprets 8bit rgb colors>
xtermMapRGB8bitComponent :: Word8
                         -- ^ input values are in
                         -- <https://en.wikipedia.org/wiki/ANSI_escape_code#Colors [0..5]>
                         -> Word8
                         -- ^ output is in range [0..255]
xtermMapRGB8bitComponent 0 = 0
xtermMapRGB8bitComponent n = 55 + n * 40

-- | How
-- <https://jonasjacek.github.io/colors/ xterm interprets 8bit grayscale colors>
xtermMapGray8bitComponent :: Word8
                          -- ^ input values are in
                          -- <https://en.wikipedia.org/wiki/ANSI_escape_code#Colors [0..23]>
                          -> Word8
                          -- ^ output is in range [0..255]
xtermMapGray8bitComponent v = 8 + 10 * v
