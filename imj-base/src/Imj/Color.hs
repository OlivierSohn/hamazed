{-# LANGUAGE NoImplicitPrelude #-}

{- |

Functions to create
<https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 8-bit ANSI colors>.

-}

module Imj.Color (
  -- * Single colors
  {- | 'rgb' creates a color in RGB color space, with 6x6x6 different possible colors.

  'gray' creates a grayscale color in RGB space with 24 shades.
  -}
    rgb
  , gray
  -- * Interpolation
  {- | We can interpolate between two single colors in RGB space, provided that
  they were both created either using 'rgb' or both created using 'gray'.-}
  , module Imj.Color.Interpolate
  -- * Layered colors
  , onBlack
  , whiteOnBlack
  -- * Predefined colors
  , white
  , black
  , red
  , green
  , magenta
  , cyan
  , yellow
  , blue
  -- * Utilities
  , xtermMapGray8bitComponent
  , xtermMapRGB8bitComponent
  , module Imj.Color.Types
) where

import           Imj.Prelude

import           Imj.Color.Interpolate
import           Imj.Color.Types

{-# INLINE onBlack #-}
-- | Creates a 'LayeredColor' with a black background color.
onBlack :: Color8 Foreground -> LayeredColor
onBlack = LayeredColor (rgb 0 0 0)

{-# INLINE whiteOnBlack #-}
-- | Creates a 'LayeredColor' with white foreground and black background color.
whiteOnBlack :: LayeredColor
whiteOnBlack = onBlack white

-- | Creates a rgb 'Color8' as defined in
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit ANSI 8-bit colors>
--
-- Input components are expected to be in range [0..5]
rgb :: Word8
    -- ^ red component in [0..5]
    -> Word8
    -- ^ green component in [0..5]
    -> Word8
    -- ^ blue component in [0..5]
    -> Color8 a
rgb r g b
  | r >= 6 || g >= 6 || b >= 6 = error "out of range"
  | otherwise = Color8 $ fromIntegral $ 16 + 36 * r + 6 * g + b


-- | Creates a gray 'Color8' as defined in
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit ANSI 8-bit colors>
--
-- Input is expected to be in the range [0..23] (from darkest to lightest)
gray :: Word8
     -- ^ gray value in [0..23]
     -> Color8 a
gray i
  | i >= 24 = error "out of range gray"
  | otherwise      = Color8 $ fromIntegral (i + 232)

red, green, blue, yellow, magenta, cyan, white, black :: Color8 a
red   = rgb 5 0 0
green = rgb 0 5 0
blue  = rgb 0 0 5
yellow = rgb 5 5 0
magenta = rgb 5 0 5
cyan = rgb 0 5 5
white = rgb 5 5 5
black = rgb 0 0 0


-- | how xterm interprets 8bit rgb colors (deduced from https://jonasjacek.github.io/colors/)
xtermMapRGB8bitComponent :: Word8
                         -- ^ input values are in range [0..5]
                         -- (the admissible range for rgb components of 8 bit
                         -- rgb ANSI colors, cf.
                         -- https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)
                         -> Word8
                         -- ^ output is in range [0..255]
xtermMapRGB8bitComponent 0 = 0
xtermMapRGB8bitComponent n = 55 + n * 40

-- | how xterm interprets 8bit grayscale colors (deduced from https://jonasjacek.github.io/colors/)
xtermMapGray8bitComponent :: Word8
                         -- ^ input values are in range [0..23]
                         -- (the admissible range for gray component of 8 bit
                         -- grayscale ANSI colors, cf.
                         -- https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)
                          -> Word8
                          -- ^ output is in range [0..255]
xtermMapGray8bitComponent v = 8 + 10 * v
