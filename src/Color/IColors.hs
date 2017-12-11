{-# LANGUAGE NoImplicitPrelude #-}

module Color.IColors
              ( IColors(..)
              , mkIColors
              , mkColors
              , module Interpolation
              ) where

import           Imajuscule.Prelude

import           Color.Types
import           Interpolation

import           Color.IColor8Code


{-# INLINE mkIColors #-}
mkIColors :: Colors -> IColors
mkIColors (Colors b f) =
  IColors (IColor8Code b) (IColor8Code f)

{-# INLINE mkColors #-}
mkColors :: IColors -> Colors
mkColors (IColors (IColor8Code b) (IColor8Code f)) =
  Colors b f

data IColors = IColors {
    _icolorBg :: !IColor8Code
  , _icolorFg :: !IColor8Code
} deriving(Show)

-- on IColors instead of Colors to avoid orphan instance
-- | First interpolate background color, then foreground color
instance DiscretelyInterpolable IColors where
  distance (IColors bg fg) (IColors bg' fg') =
    succ $ pred (distance bg bg') + pred (distance fg fg')

  interpolate (IColors bg fg) (IColors bg' fg') i
    | i < lastBgFrame = IColors (interpolate bg bg' i) fg
    | otherwise       = IColors bg' $ interpolate fg fg' $ i - lastBgFrame
    where
      lastBgFrame = pred $ distance bg bg'
