--{-# LANGUAGE PatternSynonyms #-}

module Render.Backends.Internal.BufferCell
          ( BufferCell(..)
          , getFg
          , getBg
          , mkBufferCell
          , expand
          ) where

import           Data.Bits(shiftL, shiftR, (.&.), (.|.))
import           Data.Char( chr, ord )
import           Data.Word( Word32, Word8 )

import           System.Console.ANSI( Color8Code(..) )

-- Curently we support chars between 0 and 255, and have a Word32 in BufferCell
-- TODO support all Unicode chars, and propose 2 buffercells (Word32 / Word64)
newtype BufferCell = BufferCell Word32 deriving (Eq)

{-# INLINE secondWord8 #-}
secondWord8 :: Word32 -> Word8
secondWord8 w = fromIntegral $ (w `shiftR` 16) .&. 0xFF

{-# INLINE thirdWord8 #-}
thirdWord8 :: Word32 -> Word8
thirdWord8 w = fromIntegral $ (w `shiftR` 8) .&. 0xFF

{-# INLINE fourthWord8 #-}
fourthWord8 :: Word32 -> Word8
fourthWord8 w = fromIntegral $ w .&. 0xFF

{-# INLINE getFg #-}
getFg :: BufferCell -> Color8Code
getFg (BufferCell w) = Color8Code $ secondWord8 w

{-# INLINE getBg #-}
getBg :: BufferCell -> Color8Code
getBg (BufferCell w) = Color8Code $ thirdWord8 w

{-# INLINE getChr #-}
getChr :: BufferCell -> Char
getChr (BufferCell w) = chr $ fromIntegral $ fourthWord8 w

mkBufferCell :: Color8Code -> Color8Code -> Char -> BufferCell
mkBufferCell (Color8Code fg') (Color8Code bg') char =
  let fg = fromIntegral fg'
      bg = fromIntegral bg'
      c = fromIntegral $ ord char
  in BufferCell $ c .|. (bg `shiftL` 8) .|. (fg `shiftL` 16)

-- TODO replace by using PatternSynonyms (cf https://www.reddit.com/r/haskellquestions/comments/7i6hi5/optimizing_memory_usage_array_of_unboxed_values/)
expand :: BufferCell -> (Color8Code, Color8Code, Char)
expand w = (getFg w, getBg w, getChr w)
