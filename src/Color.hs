{-# LANGUAGE NoImplicitPrelude #-}

{- |

Functions to create
<https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 8-bit ANSI colors>.

-}

module Color (
  -- * Single colors
    rgb
  , gray
  , Color8Code(..)
  -- * Layered colors
  , onBlack
  , whiteOnBlack
  , LayeredColor(..)
  -- * Predefined colors
  , white
  , black
  , red
  , green
  , magenta
  , cyan
  , yellow
  , blue
) where

import           Imajuscule.Prelude

import           System.Console.ANSI( Color8Code(..) )

import           Color.Types

{-# INLINE onBlack #-}
-- | Creates a 'LayeredColor' with a black background color.
onBlack :: Color8Code -> LayeredColor
onBlack = LayeredColor (rgb 0 0 0)

{-# INLINE whiteOnBlack #-}
-- | Creates a 'LayeredColor' with white foreground and black background color.
whiteOnBlack :: LayeredColor
whiteOnBlack = onBlack white

-- | Creates a rgb 'Color8Code' as defined in
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit ANSI 8-bit colors>
--
-- Input components are expected to be in range [0..5]
rgb :: Word8
    -- ^ red component in [0..5]
    -> Word8
    -- ^ green component in [0..5]
    -> Word8
    -- ^ blue component in [0..5]
    -> Color8Code
rgb r g b
  | r >= 6 || g >= 6 || b >= 6 = error "out of range"
  | otherwise = Color8Code $ fromIntegral $ 16 + 36 * r + 6 * g + b


-- | Creates a gray 'Color8Code' as defined in
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit ANSI 8-bit colors>
--
-- Input is expected to be in the range [0..23] (from darkest to lightest)
gray :: Word8
     -- ^ gray value in [0..23]
     -> Color8Code
gray i
  | i >= 24 = error "out of range gray"
  | otherwise      = Color8Code $ fromIntegral (i + 232)


red, green, blue, yellow, magenta, cyan, white, black :: Color8Code
red   = rgb 5 0 0
green = rgb 0 5 0
blue  = rgb 0 0 5
yellow = rgb 5 5 0
magenta = rgb 5 0 5
cyan = rgb 0 5 5
white = rgb 5 5 5
black = rgb 0 0 0
