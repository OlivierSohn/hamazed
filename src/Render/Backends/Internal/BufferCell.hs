
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
import           Data.Word( Word64, Word32, Word8 )

import           System.Console.ANSI( Color8Code(..) )

-- Curently we support chars between 0 and 255, TODO support more Unicode chars
-- Word64 is optimal: there is no wasted space when unboxed, cf. https://wiki.haskell.org/GHC/Memory_Footprint
type BackFrontCells = Word64 -- front cell is in the high bits, back cell in the low bits
type Cell           = Word32

{-# INLINE secondWord8 #-}
secondWord8 :: Word32 -> Word8
secondWord8 w = fromIntegral $ (w `shiftR` 16) .&. 0xFF

{-# INLINE thirdWord8 #-}
thirdWord8 :: Word32 -> Word8
thirdWord8 w = fromIntegral $ (w `shiftR` 8) .&. 0xFF

{-# INLINE fourthWord8 #-}
fourthWord8 :: Word32 -> Word8
fourthWord8 w = fromIntegral $ w .&. 0xFF

{-# INLINE getForegroundColor #-}
getForegroundColor :: Cell -> Color8Code
getForegroundColor w = Color8Code $ secondWord8 w

{-# INLINE getBackgroundColor #-}
getBackgroundColor :: Cell -> Color8Code
getBackgroundColor w = Color8Code $ thirdWord8 w

{-# INLINE getCharacter #-}
getCharacter :: Cell -> Char
getCharacter w = chr $ fromIntegral $ fourthWord8 w

-- TODO replace by using PatternSynonyms (cf https://www.reddit.com/r/haskellquestions/comments/7i6hi5/optimizing_memory_usage_array_of_unboxed_values/)
expand :: Cell -> (Color8Code, Color8Code, Char)
expand w = (getForegroundColor w
           ,getBackgroundColor w
           ,getCharacter w)

{-# INLINE mkCell #-}
mkCell :: Color8Code -> Color8Code -> Char -> Cell
mkCell (Color8Code fg') (Color8Code bg') char =
  let fg = fromIntegral fg'
      bg = fromIntegral bg'
      c = fromIntegral $ ord char
  in c .|. (bg `shiftL` 8) .|. (fg `shiftL` 16)

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
