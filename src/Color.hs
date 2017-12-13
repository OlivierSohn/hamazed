{-# LANGUAGE NoImplicitPrelude #-}

{- |

Functions to create
<https://en.wikipedia.org/wiki/ANSI_escape_code#LayeredColor 8-bit ANSI colors>.

-}

module Color (
  -- * create colors
    gray
  , rgb
  -- * create layered colors
  , onBlack
  -- * frequently used colors
  , white
  , black
  , red
  , green
  , blue
  -- * frequently used layered colors
  , whiteOnBlack
  -- * reexports
  , Color8Code(..)
  , module Color.Types
) where

import           Imajuscule.Prelude

import           System.Console.ANSI( Color8Code(..) )

import           Color.Types

{-# INLINE onBlack #-}
onBlack :: Color8Code -> LayeredColor
onBlack = LayeredColor (rgb 0 0 0)

{-# INLINE whiteOnBlack #-}
whiteOnBlack :: LayeredColor
whiteOnBlack = onBlack white

-- | Creates a rgb color as defined in
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#LayeredColor ANSI 8-bit colors>
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


-- | Creates a gray color as defined in
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#LayeredColor ANSI 8-bit colors>
--
-- Input is expected to be in the range [0..23] (from darkest to lightest)
gray :: Word8
     -- ^ in [0..23]
     -> Color8Code
gray i
  | i >= 24 = error "out of range gray"
  | otherwise      = Color8Code $ fromIntegral (i + 232)


red, green, blue, white, black :: Color8Code
red   = rgb 5 0 0
green = rgb 0 5 0
blue  = rgb 0 0 5
white = rgb 5 5 5
black = rgb 0 0 0
