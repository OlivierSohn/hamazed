{-# OPTIONS_HADDOCK hide #-}

module Imj.Color.Types
          ( Color8(..)
          , Background
          , Foreground
          , LayeredColor(..)
          -- * SGR codes
          , color8BgSGRToCode
          , color8FgSGRToCode
          --- * Reexports
          , RGB(..)
          , Word8
          ) where

import           Data.Word (Word8)

-- | Components are expected to be between 0 and 5 included.
data RGB = RGB {
    _rgbR :: {-# UNPACK #-} !Word8
  , _rgbG :: {-# UNPACK #-} !Word8
  , _rgbB :: {-# UNPACK #-} !Word8
} deriving(Eq, Show, Read)

-- | A background and a foreground
--  <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 8-bit color>.
data LayeredColor = LayeredColor {
    _colorsBackground :: {-# UNPACK #-} !(Color8 Background)
  , _colorsForeground :: {-# UNPACK #-} !(Color8 Foreground)
} deriving(Eq, Show)


data Foreground
data Background
-- | ANSI allows for a palette of up to 256 8-bit colors.
newtype Color8 a = Color8 Word8 deriving (Eq, Show, Read)


-- | Converts a 'Color8' 'Foreground' to corresponding SGR codes
color8FgSGRToCode :: Color8 Foreground -> [Int]
color8FgSGRToCode (Color8 c) =
  [38, 5, fromIntegral c]

-- | Converts a 'Color8' 'Background' to corresponding SGR codes
color8BgSGRToCode :: Color8 Background -> [Int]
color8BgSGRToCode (Color8 c) =
  [48, 5, fromIntegral c]
