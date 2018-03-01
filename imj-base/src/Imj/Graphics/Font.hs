{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Graphics.Font
    ( fontFiles
    , Fonts
    , mkFonts
    , lookupFont
    , Font(..)
    , Glyph(..)
    , encodeGlyph
    , decodeGlyph
    , textGlyph
    , gameGlyph
    , FontSpec(..)
    ) where

import           Imj.Prelude

import           Control.DeepSeq(NFData)
import           Data.Bits(shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString(ByteString)
import           Data.Char(chr, ord)
import           Data.FileEmbed(embedFile)
import           Data.Word(Word32, Word8)
import qualified Graphics.Rendering.FTGL as FTGL

import           Imj.Geo.Continuous.Types

{-| This is a font based on
<https://github.com/adobe-fonts/source-code-pro Source Code Pro Bold>,
modified to reduce the negative height
of the Pipe glyph used for hamazed game in animations. The reason I reduced
this height is so that the drawn pixels are kept within the logical square reserved
to that 'Coords', else, since I use delta rendering, there would be leftovers of
previous frames outside the boundaries. -}
fontFiles :: [ByteString]
-- Note that I don't embed the directory, I want compilation to fail if this particular file
-- is not found.
fontFiles = [$(embedFile "fonts/SrcCodPro-Bold-PipeReduced.ttf")]

-- I want O(1) access to any font. For now this is ok, as I use 2 fonts only.
-- in the future, maybe use an "Immutable Storable Vector".
data Fonts = Fonts {
    getFont0 :: {-# UNPACK #-} !Font
  , getFont1 :: {-# UNPACK #-} !Font
} deriving (Generic, Show, NFData)

mkFonts :: Font -> Fonts
mkFonts font = Fonts font font

lookupFont :: FontSpec -> Fonts -> Font
lookupFont (FontSpec 0) = getFont0
lookupFont (FontSpec 1) = getFont1
lookupFont (FontSpec n) = error $ "font index out of range : " ++ show n


data Font = Font {
    _ftglFont :: {-# UNPACK #-} !FTGL.Font
  , _offset :: {-# UNPACK #-} !(Vec2 Pos)
} deriving(Generic, Show, NFData)

newtype Glyph = Glyph Word32
  deriving(Generic, Show)
newtype FontSpec = FontSpec Word8

gameGlyph :: Char -> Glyph
gameGlyph c = encodeGlyph c $ FontSpec 0

textGlyph :: Char -> Glyph
textGlyph c = encodeGlyph c $ FontSpec 1

encodeGlyph :: Char -> FontSpec -> Glyph
encodeGlyph char (FontSpec s) =
  Glyph $ fromIntegral (ord char) .|. (fromIntegral s `shiftL` 24)

decodeGlyph :: Glyph -> (Char, FontSpec)
decodeGlyph (Glyph w) =
  (char, md)
 where
  char = chr $ fromIntegral $ w .&. 0xFFFFFF
  md = FontSpec $ fromIntegral $ w `shiftR` 24
