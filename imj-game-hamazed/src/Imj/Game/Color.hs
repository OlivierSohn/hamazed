{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Color (
  -- * default colors
    worldFrameColors
  , numberColor
  , bracketsColor
  , ammoColor
  , configColors
  , wallColors
  , airColors
  , shipColors
  , shipColorsSafe
  , shipColorSafe
  , shipColor
  , messageColor
  , neutralMessageColor
  , module Imj.Color
) where

import           Imj.Prelude

import           Imj.Game.Level.Types

import           Imj.Color


configColors :: LayeredColor
configColors = LayeredColor (gray 0) (gray 8)

wallColors :: LayeredColor
wallColors = LayeredColor (gray 0) (gray 3)

airColors :: LayeredColor
airColors = LayeredColor black white

neutralMessageColor :: LayeredColor
neutralMessageColor = onBlack $ gray 10

ammoColor :: Color8 Foreground
ammoColor = gray 14

bracketsColor :: Color8 Foreground
bracketsColor = worldFrameFgColor

messageColor :: GameStops -> LayeredColor
messageColor Won      = onBlack $ rgb 4 3 1
messageColor (Lost _) = onBlack $ gray 6

shipColors :: LayeredColor
shipColors = LayeredColor shipBgColor shipColor

shipColorsSafe :: LayeredColor
shipColorsSafe = LayeredColor shipBgColorSafe shipColorSafe

shipColor :: Color8 Foreground
shipColor = rgb 5 4 4

shipColorSafe :: Color8 Foreground
shipColorSafe = rgb 5 0 0

shipBgColor :: Color8 Background
shipBgColor = black

shipBgColorSafe :: Color8 Background
shipBgColorSafe = rgb 1 0 0

numberColor :: Int -> LayeredColor
numberColor i = onBlack $ rgb r g b
  where
    r = 5
    g = fromIntegral $ 4 + (0 + quot i 2) `mod` 2 -- [0..1] , slow changes
    b = fromIntegral $ 1 + (0 + quot i 1) `mod` 3 -- [0..2] , 2x faster changes

worldFrameFgColor :: Color8 Foreground
worldFrameFgColor = rgb 2 1 1

worldFrameColors :: LayeredColor
worldFrameColors = LayeredColor black worldFrameFgColor
