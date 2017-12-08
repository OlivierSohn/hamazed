
module Render.Backends.Internal.BufferCell
          ( Cell
          , mkCell
          , expand
          ) where

import           Data.Bits(shiftL, shiftR, (.&.), (.|.))
import           Data.Char( chr, ord )
import           Data.Word( Word64, Word32, Word16, Word8 )

import           System.Console.ANSI( Color8Code(..) )

-- Word64 is optimal: there is no wasted space when unboxed,
--   cf. https://wiki.haskell.org/GHC/Memory_Footprint
type Cell = Word64

{-# INLINE firstWord32 #-}
firstWord32 :: Word64 -> Word32
firstWord32 w = fromIntegral $ (w `shiftR` 32) .&. 0xFFFFFFFF

{-# INLINE seventhWord8 #-}
seventhWord8 :: Word64 -> Word8
seventhWord8 w = fromIntegral $ (w `shiftR` 8) .&. 0xFF

{-# INLINE eigthWord8 #-}
eigthWord8 :: Word64 -> Word8
eigthWord8 w = fromIntegral $ w .&. 0xFF

{-# INLINE getForegroundColor #-}
getForegroundColor :: Cell -> Color8Code
getForegroundColor w = Color8Code $ seventhWord8 w

{-# INLINE getBackgroundColor #-}
getBackgroundColor :: Cell -> Color8Code
getBackgroundColor w = Color8Code $ eigthWord8 w

{-# INLINE getCharacter #-}
getCharacter :: Cell -> Char
getCharacter w = chr $ fromIntegral $ firstWord32 w

{-# INLINE expand #-}
expand :: Cell -> (Color8Code, Color8Code, Char)
expand w = (getForegroundColor w
           ,getBackgroundColor w
           ,getCharacter w)

{-# INLINE mkCell #-}
mkCell :: Color8Code -> Color8Code -> Char -> Cell
mkCell (Color8Code fg') (Color8Code bg') char' =
  let fg = fromIntegral fg' :: Word16
      bg = fromIntegral bg' :: Word16
      color' = bg .|. (fg `shiftL` 8)
      color = fromIntegral color'
      char = fromIntegral $ ord char'
  in color .|. (char `shiftL` 32)
