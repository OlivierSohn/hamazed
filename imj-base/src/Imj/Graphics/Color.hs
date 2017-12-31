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
    -- ** Create a LayeredColor
  , LayeredColor(..)
  , Background
  , Foreground
  , onBlack
  , whiteOnBlack
    -- ** Predefined colors
  , white, black, red, green, magenta, cyan, yellow, blue
  ) where


import           Imj.Graphics.Color.Types

-- For reference:
{-
import           Imj.Prelude
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
-}
