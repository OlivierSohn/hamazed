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
import           Data.Data(Data(..))
import           GHC.Generics(Generic(..))

import           Imj.Data.AlmostFloat

#include "c.h"

data HarmonicProperties = HarmonicProperties {
    phase :: !AlmostFloat -- using 'AlmostFloat' instead of CFloat to have the 'Data' instance, and to take rounding errors into account.
  , volume :: !AlmostFloat
} deriving (Generic, Show, Data)
instance Binary HarmonicProperties
instance NFData HarmonicProperties
instance Eq HarmonicProperties where
  {-# INLINABLE (==) #-}
  (HarmonicProperties p v) == (HarmonicProperties p' v') =
    v == v' && (v == 0 || p == p') -- when volume is zero phase is not checked.
instance Ord HarmonicProperties where
  {-# INLINABLE compare #-}
  (HarmonicProperties p v) `compare` (HarmonicProperties p' v') =
    case compare v v' of
      EQ ->
        if v == 0
          then
            EQ
          else
            compare p p'
      other -> other

instance Storable HarmonicProperties where
    sizeOf    _ = #{size harmonicProperties_t}
    alignment _ = #{alignment harmonicProperties_t}

    poke p harmonicProperties = do
      #{poke harmonicProperties_t, phase} p $ unAlmostFloat $ phase harmonicProperties
      #{poke harmonicProperties_t, volume} p $ unAlmostFloat $ volume harmonicProperties

    peek p = do
      pha <- #{peek harmonicProperties_t, phase} p
      vol <- #{peek harmonicProperties_t, volume} p
      return $ HarmonicProperties
             (almost pha)
             (almost vol)

scaleVolume :: AlmostFloat -> HarmonicProperties -> HarmonicProperties
scaleVolume s p = p { volume = s * volume p}
