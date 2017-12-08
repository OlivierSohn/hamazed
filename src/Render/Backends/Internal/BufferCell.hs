
module Render.Backends.Internal.BufferCell
          ( Cell
          , BackFrontCells
          , mkCell
          , mkCellsWithBackFront
          , setBack
          , expand
          , splitBackFront
          ) where

import           Data.Bits(shiftL, shiftR, (.&.), (.|.))
import           Data.Char( chr, ord )
import           Data.Word( Word64, Word32, Word16, Word8 )

import           System.Console.ANSI( Color8Code(..) )

-- Curently we support unicode chars between 0x0000 and 0xFFFF, TODO support all of them (until 0xFFFFFFFF)
-- Word64 is optimal: there is no wasted space when unboxed, cf. https://wiki.haskell.org/GHC/Memory_Footprint
type BackFrontCells = Word64 -- front cell is in the high bits, back cell in the low bits
type Cell           = Word32

{-# INLINE firstWord8 #-}
firstWord8 :: Word32 -> Word8
firstWord8 w = fromIntegral $ (w `shiftR` 24) .&. 0xFF

{-# INLINE secondWord8 #-}
secondWord8 :: Word32 -> Word8
secondWord8 w = fromIntegral $ (w `shiftR` 16) .&. 0xFF

{-# INLINE secondWord16 #-}
secondWord16 :: Word32 -> Word16
secondWord16 w = fromIntegral $ w .&. 0xFFFF

{-# INLINE getForegroundColor #-}
getForegroundColor :: Cell -> Color8Code
getForegroundColor w = Color8Code $ firstWord8 w

{-# INLINE getBackgroundColor #-}
getBackgroundColor :: Cell -> Color8Code
getBackgroundColor w = Color8Code $ secondWord8 w

{-# INLINE getCharacter #-}
getCharacter :: Cell -> Char
getCharacter w = chr $ fromIntegral $ secondWord16 w

expand :: Cell -> (Color8Code, Color8Code, Char)
expand w = (getForegroundColor w
           ,getBackgroundColor w
           ,getCharacter w)

{-# INLINE mkCell #-}
mkCell :: Color8Code -> Color8Code -> Char -> Cell
mkCell (Color8Code fg') (Color8Code bg') char =
  let fg = fromIntegral fg'
      bg = fromIntegral bg'
      c' = ord char
      c = fromIntegral $ if c' <= fromIntegral (maxBound :: Word16)
            then
              c'
            else
              ord '?' -- this range of Unicode chars is not supported yet

  in c .|. (bg `shiftL` 16) .|. (fg `shiftL` 24)

{-# INLINE mkCellsWithBackFront #-}
mkCellsWithBackFront :: Cell -> Cell -> BackFrontCells
mkCellsWithBackFront back front =
  fromIntegral back .|. (fromIntegral front `shiftL` 32)

{-# INLINE setBack #-}
setBack :: BackFrontCells -> Cell -> BackFrontCells
setBack cells back = fromIntegral back .|. ((cells `shiftR` 32) `shiftL` 32)

{-# INLINE splitBackFront #-}
splitBackFront :: BackFrontCells -> (Cell, Cell)
splitBackFront cells =
  (fromIntegral cells {-no need to mask, as we convert to a Word32 -}
  ,fromIntegral $ cells `shiftR` 32)
