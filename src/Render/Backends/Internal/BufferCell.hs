
module Render.Backends.Internal.BufferCell
          ( Cell
          , mkCell
          , mkIndexedCell
          , expand
          , expandIndexed
          ) where

import           Data.Bits(shiftL, shiftR, (.&.), (.|.))
import           Data.Char( chr, ord )
import           Data.Word( Word64, Word32, Word16, Word8 )

import           Color.Types
import           Render.Backends.Internal.Types
import           Render.Types


{-# INLINE firstWord8 #-}
firstWord8 :: Word64 -> Word8
firstWord8 w = fromIntegral $ w `shiftR` 56

{-# INLINE secondWord8 #-}
secondWord8 :: Word64 -> Word8
secondWord8 w = fromIntegral $ (w `shiftR` 48) .&. 0xFF

{-# INLINE secondWord16 #-}
secondWord16 :: Word64 -> Word16
secondWord16 w = fromIntegral $ (w `shiftR` 32) .&. 0xFFFF

{-# INLINE secondWord32 #-}
secondWord32 :: Word64 -> Word32
secondWord32 = fromIntegral

{-# INLINE getForegroundColor #-}
getForegroundColor :: Cell -> Color8Code
getForegroundColor w = Color8Code $ secondWord8 w

{-# INLINE getBackgroundColor #-}
getBackgroundColor :: Cell -> Color8Code
getBackgroundColor w = Color8Code $ firstWord8 w

{-# INLINE getCharacter #-}
getCharacter :: Cell -> Char
getCharacter w = chr $ fromIntegral $ secondWord32 w

{-# INLINE getIndex #-}
getIndex :: Cell -> Dim Index
getIndex w = fromIntegral $ secondWord16 w

{-# INLINE expand #-}
expand :: Cell -> (Color8Code, Color8Code, Char)
expand w = (getBackgroundColor w
           ,getForegroundColor w
           ,getCharacter w)

{-# INLINE expandIndexed #-}
expandIndexed :: Cell -> (Color8Code, Color8Code, Dim Index, Char)
expandIndexed w =
  (getBackgroundColor w
  ,getForegroundColor w
  ,getIndex w
  ,getCharacter w)

{-# INLINE encodeColors #-}
encodeColors :: Colors -> Word16
encodeColors (Colors (Color8Code bg') (Color8Code fg')) =
  let fg = fromIntegral fg' :: Word16
      bg = fromIntegral bg' :: Word16
  in (bg `shiftL` 8) .|. fg

{-# INLINE mkIndexedCell #-}
mkIndexedCell :: Cell -> Dim Index -> Cell
mkIndexedCell cell idx' =
  cell .|. (idx `shiftL` 32)
 where
  idx = fromIntegral idx'

{-# INLINE mkCell #-}
mkCell :: Colors -> Char -> Cell
mkCell colors char' =
  let color = fromIntegral $ encodeColors colors
      char = fromIntegral $ ord char'
  in (color `shiftL` 48) .|. char
