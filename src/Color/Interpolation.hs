{-# LANGUAGE NoImplicitPrelude #-}

module Color.Interpolation
        ( IColor8Code(..)
        -- utilities
        , bresenhamRGBLength
        , bresenhamRGB
        , bresenhamColor8Length
        , bresenhamColor8
        -- | reexports
        , module Interpolation
        ) where

import           Imajuscule.Prelude

import           System.Console.ANSI.Codes( xterm256ColorToCode )
import           System.Console.ANSI(Color8Code(..), Xterm256Color(..))
import           System.Console.ANSI.Color( color8CodeToXterm256 )

import           Data.Colour.SRGB (RGB (..))

import           Geo.Discrete.Bresenham3

import           Interpolation

import           Math
import           Util

newtype IColor8Code = IColor8Code Color8Code deriving (Show)

-- on IColor8Code instead of Color8Code to avoid orphan instance
instance DiscretelyInterpolable IColor8Code where
  distance (IColor8Code c) (IColor8Code c') =
    bresenhamColor8Length c c'

  interpolate (IColor8Code c) (IColor8Code c') i
    | c == c' = IColor8Code c
    | otherwise =
        let lastFrame = pred $ fromIntegral $ bresenhamColor8Length c c'
            -- TODO measure if "head . drop (pred n)"" is more optimal than "!! n"
            index = clamp i 0 lastFrame
        in IColor8Code . head . drop index $ bresenhamColor8 c c'

-- | Interpolations betwee 2 rgb or 2 grays are well-defined, whereas
--   other interpolations fallback on raw Color8Code interpolation which
--   has little visual meaning. To improve on this, we could define conversion
--   functions between different representations in the future.

{-# INLINABLE bresenhamColor8Length #-}
bresenhamColor8Length :: Color8Code -> Color8Code -> Int
bresenhamColor8Length c@(Color8Code v) c'@(Color8Code v')
  | c == c' = 1
  | otherwise =
      case (color8CodeToXterm256 c, color8CodeToXterm256 c') of
        (RGBColor rgb1, RGBColor rgb2) -> bresenhamRGBLength rgb1 rgb2
        (GrayColor g1, GrayColor g2) -> 1 + fromIntegral (abs (g2 - g1))
        colors -> error $ "cannot get length between colors " ++ show colors

{-# INLINABLE bresenhamColor8 #-}
bresenhamColor8 :: Color8Code -> Color8Code -> [Color8Code]
bresenhamColor8 c@(Color8Code v) c'@(Color8Code v')
  | c == c' = [c]
  | otherwise =
      case (color8CodeToXterm256 c, color8CodeToXterm256 c') of
        (RGBColor rgb1, RGBColor rgb2) ->
          map (xterm256ColorToCode . RGBColor) $ bresenhamRGB rgb1 rgb2
        (GrayColor g1, GrayColor g2) ->
          map Color8Code $ range g1 g2
        colors -> error $ "cannot interpolate between colors " ++ show colors

{-# INLINABLE bresenhamRGBLength #-}
bresenhamRGBLength :: RGB Word8 -> RGB Word8 -> Int
bresenhamRGBLength (RGB r g b) (RGB r' g' b') =
  bresenham3Length (fromIntegral r,fromIntegral g,fromIntegral b) (fromIntegral r',fromIntegral g',fromIntegral b')

{-# INLINABLE bresenhamRGB #-}
bresenhamRGB :: RGB Word8 -> RGB Word8 -> [RGB Word8]
bresenhamRGB (RGB r g b) (RGB r' g' b') =
  map
    (\(x,y,z) -> RGB (fromIntegral x) (fromIntegral y) (fromIntegral z))
    $ bresenham3 (fromIntegral r,fromIntegral g,fromIntegral b)
                 (fromIntegral r',fromIntegral g',fromIntegral b')
