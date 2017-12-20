{-# LANGUAGE NoImplicitPrelude #-}

-- | 'ILayeredColor' wraps 'LayeredColor' to define a DiscretelyInterpolable
--  instance.
module Color.ILayeredColor
              ( ILayeredColor(..)
              -- * Conversions
              , mkIColors
              , mkColors
              -- * Reexports
              , module Interpolation
              ) where

import           Imajuscule.Prelude

import           Color.IColor8
import           Color.Types
import           Interpolation


{-# INLINE mkIColors #-}
mkIColors :: LayeredColor -> ILayeredColor
mkIColors (LayeredColor b f) =
  ILayeredColor (IColor8 b) (IColor8 f)

{-# INLINE mkColors #-}
mkColors :: ILayeredColor -> LayeredColor
mkColors (ILayeredColor (IColor8 b) (IColor8 f)) =
  LayeredColor b f

data ILayeredColor = ILayeredColor {
    _icolorBg :: !(IColor8 Background)
    -- ^ Background color
  , _icolorFg :: !(IColor8 Foreground)
    -- ^ Foreground color
} deriving(Show)

-- TODO bresenham 6 to interpolate foreground and background at the same time:
-- https://nenadsprojects.wordpress.com/2014/08/08/multi-dimensional-bresenham-line-in-c/
-- | First interpolate background color, then foreground color
instance DiscretelyInterpolable ILayeredColor where
  distance (ILayeredColor bg fg) (ILayeredColor bg' fg') =
    succ $ pred (distance bg bg') + pred (distance fg fg')

  interpolate (ILayeredColor bg fg) (ILayeredColor bg' fg') i
    | i < lastBgFrame = ILayeredColor (interpolate bg bg' i) fg
    | otherwise       = ILayeredColor bg' $ interpolate fg fg' $ i - lastBgFrame
    where
      lastBgFrame = pred $ distance bg bg'
