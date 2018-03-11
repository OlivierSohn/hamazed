{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

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
  , dangerousNumberColor
  , unreachableNumberColor
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
  , neutralMessageColorFg
  -- ** Cyclic colors
  , ColorCycle
  , ColorCycles(..)
  , mkColorCycles
  , rotateHues
  , cycleColors
  , refShipColor
  -- ** Combining functions
  , sumFrameParticleIndex
  , onlyFrame
  , onlyParticleIndex
  -- * Reexports
  , module Imj.Graphics.Color
  ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Level.Types
import           Imj.Graphics.ParticleSystem.Design.Types

import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Graphics.Color
import           Imj.Iteration

data ColorCycle a = ColorCycle {-# UNPACK #-} !(Color8 a) {-# UNPACK #-} !(Color8 a)
  deriving(Generic, Show, Eq)
instance Binary (ColorCycle a)

data ColorCycles = ColorCycles {
    outer1, outer2, wall1, wall2, laser :: {-# UNPACK #-} !(ColorCycle Foreground)
} deriving(Generic, Show, Eq)
instance Binary ColorCycles

mkColorCycles :: Color8 Foreground -> ColorCycles
mkColorCycles c =
  ColorCycles (r outer1ColorCycle) (r outer2ColorCycle) (r wall1ColorCycle) (r wall2ColorCycle) (r laserColorCycle)
 where
  refHue = fromMaybe (error "unexpected") $ hue refShipColor
  thisHue = fromMaybe refHue $ hue c -- gray ships are assimilated to refShipColor
  r = rotateHues $ thisHue - refHue

rotateHues :: Float -> ColorCycle a -> ColorCycle a
rotateHues dh (ColorCycle a b) =
  ColorCycle (rotateHue dh a) (rotateHue dh b)

onlyFrame :: Frame -> ParticleIndex -> Int
onlyFrame (Frame f) _ = f

onlyParticleIndex :: Frame -> ParticleIndex -> Int
onlyParticleIndex _ (ParticleIndex i) = i

sumFrameParticleIndex :: Frame -> ParticleIndex -> Int
sumFrameParticleIndex (Frame f) (ParticleIndex i) = f + i

cycleColors :: (Frame -> ParticleIndex -> Int)
            -> ColorCycle Foreground
            -> Colorization
cycleColors combine (ColorCycle from to) f i =
  onBlack $ interpolateCyclic from to $ combine f i

-- | We consider that 'ColorCycle' values in this module were hand-tuned in the context
-- of this reference ship color.
--
-- To adapt a 'ColorCycle' value for another ship color, we compute the 'hue' difference between
-- this 'refShipColor' and the other ship color, and apply the hue difference to 'ColorCycle' values
-- using 'rotateHues'.
refShipColor :: Color8 Foreground
refShipColor = rgb 4 2 1

outer1ColorCycle, outer2ColorCycle, wall1ColorCycle, wall2ColorCycle, laserColorCycle :: ColorCycle Foreground
outer1ColorCycle = ColorCycle (rgb 5 2 4) (rgb 1 1 2)
outer2ColorCycle = ColorCycle (rgb 4 2 1) (rgb 3 1 0)
wall1ColorCycle  = ColorCycle (rgb 3 2 2) (rgb 3 1 0)
wall2ColorCycle  = ColorCycle (rgb 4 2 1) (rgb 3 2 2)
laserColorCycle  = ColorCycle (rgb 3 2 4) (rgb 3 2 2)

darkConfigFgColor, configFgColor, neutralMessageColorFg, ammoColor :: Color8 Foreground
darkConfigFgColor = gray 4
configFgColor = gray 8
neutralMessageColorFg = gray 10
ammoColor = gray 14

neutralMessageColor :: LayeredColor
neutralMessageColor = onBlack neutralMessageColorFg


configColors :: LayeredColor
configColors = LayeredColor black configFgColor

wallColors :: LayeredColor
wallColors = LayeredColor (gray 0) (gray 3)

-- | Outer meaning outside the world
outerWallsColors :: LayeredColor
outerWallsColors = LayeredColor black (gray 1)

airColors :: LayeredColor
airColors = LayeredColor black black

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
numberColor :: Int -> Color8 Foreground
numberColor i =
  rgb r g b
 where
  (r,g,b) = numColor i

dangerousNumberColor :: Int -> Color8 Foreground
dangerousNumberColor i =
  rgb r 0 b
 where
  (r,_,b) = numColor i

unreachableNumberColor :: Int -> Color8 Foreground
unreachableNumberColor i =
  gray $ g + b
 where
  (_,g,b) = numColor i

numColor :: Int -> (Int, Int, Int)
numColor i = (r,g,b)
 where
  r = 5
  g = fromIntegral $ 4 + (0 + quot i 2) `mod` 2 -- [0..1] , slow changes
  b = fromIntegral $ 1 + (0 + quot i 1) `mod` 3 -- [0..2] , 2x faster changes

worldFrameFgColor :: Color8 Foreground
worldFrameFgColor = rgb 2 1 1

worldFrameColors :: LayeredColor
worldFrameColors = LayeredColor black worldFrameFgColor
