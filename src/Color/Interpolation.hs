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

newtype IColor8Code = IColor8Code Color8Code deriving (Show)

-- on IColor8Code instead of Color8Code to avoid orphan instance
instance DiscretelyInterpolable IColor8Code where
  distance (IColor8Code c) (IColor8Code c') =
    bresenhamColor8Length c c'

  interpolate (IColor8Code c) (IColor8Code c') i
    | c == c' = IColor8Code c
    | otherwise =
        let l = fromIntegral $ bresenhamColor8Length c c'
            -- TODO measure if "head . drop (pred n)"" is more optimal than "!! n"
            index = clamp i 0 (pred l)
        in IColor8Code . head . drop (pred index) $ bresenhamColor8 c c'


{-# INLINABLE bresenhamColor8Length #-}
bresenhamColor8Length :: Color8Code -> Color8Code -> Int
bresenhamColor8Length c c' =
  case (color8CodeToXterm256 c, color8CodeToXterm256 c') of
    (RGBColor rgb1, RGBColor rgb2) -> bresenhamRGBLength rgb1 rgb2
    _ -> error "not supported"

{-# INLINABLE bresenhamColor8 #-}
bresenhamColor8 :: Color8Code -> Color8Code -> [Color8Code]
bresenhamColor8 c c' =
  case (color8CodeToXterm256 c, color8CodeToXterm256 c') of
    (RGBColor rgb1, RGBColor rgb2) ->
      map (xterm256ColorToCode . RGBColor) $ bresenhamRGB rgb1 rgb2
    _ -> error "not supported"

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
