{-# LANGUAGE NoImplicitPrelude #-}

-- | This module defines colors of Hamazed game elements.

module Imj.Game.Hamazed.Color (
  -- * Ship colors
    shipColor
  , shipBgColorSafe
  , shipBgColor
  , shipColors
  , shipColorsSafe
  -- * Numbers colors
  , numberColor
  -- * Materials colors
  , airColors
  , wallColors
  , outerWallsColors
  -- * UI colors
  , worldFrameFgColor
  , worldFrameColors
  , ammoColor
  , bracketsColor
  , pendingTextColors
  , pendingTextColorsInactive
  , pendingTextColorsEdited
  -- ** Text colors
  , configColors
  , configFgColor
  , darkConfigFgColor
  , messageColor
  , neutralMessageColor
  -- ** Cyclic colors
  , cycleOuterColors1
  , cycleOuterColors2
  , cycleWallColors1
  , cycleWallColors2
  , cycleLaserColors
  -- * Reexports
  , module Imj.Graphics.Color
  ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Level.Types
import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Graphics.Color
import           Imj.Iteration


cycleOuterColors1 :: Frame -> LayeredColor
cycleOuterColors1 (Frame frame) =
  LayeredColor (gray 0) $ interpolateCyclic (rgb 5 2 4) (rgb 1 1 2) frame

cycleOuterColors2 :: Frame -> LayeredColor
cycleOuterColors2 (Frame frame) =
  LayeredColor (gray 0) $ interpolateCyclic (rgb 4 2 1) (rgb 3 1 0) frame

cycleWallColors1 :: Frame -> LayeredColor
cycleWallColors1 (Frame frame) =
  LayeredColor (gray 0) $ interpolateCyclic (rgb 3 2 2) (rgb 3 1 0) frame

cycleWallColors2 :: Frame -> LayeredColor
cycleWallColors2 (Frame frame) =
  LayeredColor (gray 0) $ interpolateCyclic (rgb 4 2 1) (rgb 3 2 2) frame

cycleLaserColors :: Frame -> LayeredColor
cycleLaserColors (Frame frame) =
  LayeredColor (gray 0) $ interpolateCyclic (rgb 3 2 4) (rgb 3 2 2) frame

configFgColor :: Color8 Foreground
configFgColor = gray 8

darkConfigFgColor :: Color8 Foreground
darkConfigFgColor = gray 4

configColors :: LayeredColor
configColors = LayeredColor (gray 0) configFgColor

wallColors :: LayeredColor
wallColors = LayeredColor (gray 0) (gray 3)

-- | Outer meaning outside the world
outerWallsColors :: LayeredColor
outerWallsColors = LayeredColor (rgb 0 0 0) (gray 1)

airColors :: LayeredColor
airColors = LayeredColor black black

neutralMessageColor :: LayeredColor
neutralMessageColor = onBlack $ gray 10

ammoColor :: Color8 Foreground
ammoColor = gray 14

bracketsColor :: Color8 Foreground
bracketsColor = worldFrameFgColor

messageColor :: LevelOutcome -> LayeredColor
messageColor Won      = onBlack $ rgb 4 3 1
messageColor (Lost _) = onBlack $ gray 6

pendingTextColors :: LayeredColor
pendingTextColors = onBlack $ gray 12

pendingTextColorsInactive :: LayeredColor
pendingTextColorsInactive = pendingTextColors

pendingTextColorsEdited :: LayeredColor
pendingTextColorsEdited = LayeredColor (gray 5) $ gray 12

shipColors :: LayeredColor
shipColors = LayeredColor shipBgColor shipColor

shipColorsSafe :: LayeredColor
shipColorsSafe = LayeredColor shipBgColorSafe shipColor

shipColor :: Color8 Foreground
shipColor = rgb 5 4 4

shipBgColor :: Color8 Background
shipBgColor = black

shipBgColorSafe :: Color8 Background
shipBgColorSafe = gray 3

-- | Cycles through the 6 colors of the cube delimited in RGB space by
-- (5,4,1) and (5,5,3).
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
