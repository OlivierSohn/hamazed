{-# LANGUAGE NoImplicitPrelude #-}

{--based on https://en.wikipedia.org/wiki/ANSI_escape_code#Colors--}

module Color (
  -- default colors
    worldFrameColor
  , colorFromFrame
  , numberColor
  , bracketsColor
  , ammoColor
  , wallColors
  , airColors
  , shipColors
  , shipColorsSafe
  , shipColorSafe
  , shipColor
  , messageColor
  , neutralMessageColor
  -- 4-bit colors
  , white
  , black
    -- create colors
  , gray
  , rgb
) where

import           Imajuscule.Prelude

import           System.Console.ANSI.Codes( xterm256ColorToCode )
import           System.Console.ANSI(Color8Code(..), Xterm256Color(..))

import           Data.Colour.SRGB --(Colour, RGB (..), sRGB, toSRGB)

import           Animation.Types
import           Game.Level.Types

wallColors :: (Color8Code, Color8Code)
wallColors = (gray 3, gray 0)

airColors :: (Color8Code, Color8Code)
airColors = (white, black)

neutralMessageColor :: Color8Code
neutralMessageColor = gray 10

ammoColor :: Color8Code
ammoColor = gray 14

bracketsColor :: Color8Code
bracketsColor = worldFrameColor

messageColor :: GameStops -> Color8Code
messageColor Won      = rgb 4 3 1
messageColor (Lost _) = gray 6

shipColors :: (Color8Code, Color8Code)
shipColors = (shipColor, shipBgColor)

shipColorsSafe :: (Color8Code, Color8Code)
shipColorsSafe = (shipColorSafe, shipBgColorSafe)

shipColor :: Color8Code
shipColor = rgb 5 4 4

shipColorSafe :: Color8Code
shipColorSafe = rgb 5 0 0

shipBgColor :: Color8Code
shipBgColor = black

shipBgColorSafe :: Color8Code
shipBgColorSafe = rgb 1 0 0

numberColor :: Int -> Color8Code
numberColor i = xterm256ColorToCode $ RGBColor (RGB r g b)
  where
    r = 5
    g = fromIntegral $ 4 + (0 + quot i 2) `mod` 2 -- [0..1] , slow changes
    b = fromIntegral $ 1 + (0 + quot i 1) `mod` 3 -- [0..2] , 2x faster changes

colorFromFrame :: Frame -> Color8Code
colorFromFrame (Frame f) = xterm256ColorToCode $ RGBColor (RGB r g b)
  where
    r = assert (f >= 0) 4
    g = fromIntegral $ (0 + quot f 6) `mod` 2 -- [0..1] , slow changes
    b = fromIntegral $ (0 + quot f 3) `mod` 3 -- [0..2] , 2x faster changes

worldFrameColor :: Color8Code
worldFrameColor = rgb 2 1 1

--------------------------------------------------------------------------------

white :: Color8Code
white = Color8Code 15

black :: Color8Code
black = Color8Code 0

firstGray :: Word8
firstGray = 232

grayRange :: Word8
grayRange = 24

firstRGB :: Word8
firstRGB = 16

rgbRange :: Word8
rgbRange = 6

gray :: Word8 -> Color8Code
gray i
  | i >= grayRange = error "out of range gray"
  | otherwise      = Color8Code $ fromIntegral (i + firstGray)

rgb :: Word8 -> Word8 -> Word8 -> Color8Code
rgb r g b
  | r >= rgbRange || g >= rgbRange || b >= rgbRange = error "out of range rgb"
  | otherwise = Color8Code $ fromIntegral $ firstRGB + 36 * r + 6 * g + b
