{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Audio.Harmonics
      ( HarmonicProperties(..)
      , scaleVolume
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

scaleVolume :: Float -> HarmonicProperties -> HarmonicProperties
scaleVolume s p = p { volume = s * volume p}
