{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Imj.Audio.Harmonics
      ( HarmonicProperties(..)
      ) where

import           Foreign
import           Data.Binary
import           Control.DeepSeq
import           Control.Monad (ap)
import           Data.Data(Data(..))
import           GHC.Generics(Generic(..))

#include "c.h"

data HarmonicProperties = HarmonicProperties {
    phase :: !Float -- using Float instead of CFloat to have the 'Data' instance
  , volume :: !Float
} deriving (Generic, Show, Ord, Eq, Data)
instance Binary HarmonicProperties
instance NFData HarmonicProperties

instance Storable HarmonicProperties where
    sizeOf    _ = #{size harmonicProperties_t}
    alignment _ = alignment (undefined :: Float)

    poke p harmonicProperties_t = do
      #{poke harmonicProperties_t, phase} p $ phase harmonicProperties_t
      #{poke harmonicProperties_t, volume} p $ volume harmonicProperties_t

    peek p = return HarmonicProperties
             `ap` (#{peek harmonicProperties_t, phase} p)
             `ap` (#{peek harmonicProperties_t, volume} p)
{-
foreign import ccall "c.h harmonicProperties_test" crgbTest :: Ptr HarmonicProperties -> CSize -> IO ();

rgbTest :: [RGB] -> IO [RGB]
rgbTest rgbs = withArray rgbs $ \ptr ->
               do
                 crgbTest ptr (fromIntegral (length rgbs))
                 peekArray (length rgbs) ptr

rgbAlloc :: [RGB] -> IO (Ptr RGB)
rgbAlloc rgbs = newArray rgbs

rgbPeek :: Ptr RGB -> Int -> IO [RGB]
rgbPeek rgbs l = peekArray l rgbs

rgbTest2 :: Ptr RGB -> Int -> IO ()
rgbTest2 ptr l =
    do
      crgbTest ptr (fromIntegral l)
      return ()
-}
