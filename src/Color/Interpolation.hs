module Color.Interpolation
        ( IColor8Code(..)
        -- | reexports
        , module Interpolation
        ) where

import System.Console.ANSI( Color8Code(..), Xterm256Color(..) )
import System.Console.ANSI.Color( color8CodeToXterm256, xterm256ColorToCode )

import Color

import Interpolation

import Math

newtype IColor8Code = IColor8Code Color8Code deriving (Show)

-- on IColor8Code instead of Color8Code to avoid orphan instance
instance DiscretelyInterpolable IColor8Code where
  distance (IColor8Code c) (IColor8Code c') =
    case (color8CodeToXterm256 c, color8CodeToXterm256 c') of
      (RGBColor rgb1, RGBColor rgb2) -> bresenhamRGBLength rgb1 rgb2
      _ -> error "not supported"

  interpolate (IColor8Code c) (IColor8Code c') i =
    case (color8CodeToXterm256 c, color8CodeToXterm256 c') of
      (RGBColor rgb1, RGBColor rgb2) ->
        let l = fromIntegral $ bresenhamRGBLength rgb1 rgb2
        in  IColor8Code $ xterm256ColorToCode $ RGBColor $ bresenhamRGB rgb1 rgb2 !! clamp i 0 (l-1)
      _ -> error "not supported"
