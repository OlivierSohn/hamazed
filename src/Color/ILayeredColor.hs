{-# LANGUAGE NoImplicitPrelude #-}

-- | Exposes a wrapper type on LayeredColor that has a DiscretelyInterpolable
--  instance.
module Color.ILayeredColor
              ( ILayeredColor(..)
              , mkIColors
              , mkColors
              , module Interpolation
              ) where

import           Imajuscule.Prelude

import           Color.Types
import           Interpolation

import           Color.IColor8Code


{-# INLINE mkIColors #-}
mkIColors :: LayeredColor -> ILayeredColor
mkIColors (LayeredColor b f) =
  ILayeredColor (IColor8Code b) (IColor8Code f)

{-# INLINE mkColors #-}
mkColors :: ILayeredColor -> LayeredColor
mkColors (ILayeredColor (IColor8Code b) (IColor8Code f)) =
  LayeredColor b f

data ILayeredColor = ILayeredColor {
    _icolorBg :: !IColor8Code
  , _icolorFg :: !IColor8Code
} deriving(Show)

-- | First interpolate background color, then foreground color
instance DiscretelyInterpolable ILayeredColor where
  distance (ILayeredColor bg fg) (ILayeredColor bg' fg') =
    succ $ pred (distance bg bg') + pred (distance fg fg')

  interpolate (ILayeredColor bg fg) (ILayeredColor bg' fg') i
    | i < lastBgFrame = ILayeredColor (interpolate bg bg' i) fg
    | otherwise       = ILayeredColor bg' $ interpolate fg fg' $ i - lastBgFrame
    where
      lastBgFrame = pred $ distance bg bg'
