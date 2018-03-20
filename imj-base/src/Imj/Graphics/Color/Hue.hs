
{-# LANGUAGE NoImplicitPrelude #-}

{-| Every 'Color8' can be decomposed in a /saturated/ part and a /gray/ part.

Throughout this module, the notion of hue applies to the /saturated/ part.
-}

module Imj.Graphics.Color.Hue
          ( rotateHue
          , hue
          , countHuesOfSameIntensity
          , sameIntensityHues
          ) where

import           Imj.Prelude

import           Imj.Graphics.Color.Types
import           Imj.Graphics.Color.Hue.Internal

{-# INLINE hue #-}
hue :: Color8 a -> Maybe Float
hue = hue' . color8CodeToXterm256

-- | Generates a color that has the same /gray/ component as the input color,
-- and whose /saturated/ component has the same intensity (ie max color component)
-- as the /saturated/ component of the input color.
rotateHue :: Float
          -- ^ The angle, in Turns (as defined
          -- <https://en.wikipedia.org/wiki/Angular_unit here>), ie @1@ is a full circle.
          -- Due to the discrete nature of the 6x6x6 color space in which 'Color8' live,
          -- angles will be rounded, hence this function called with 2 different but close
          -- angles may generate the same color.
          -- You may want to use 'countHuesOfSameIntensity' to compute the minimum angle that will produce
          -- a different color.
          -> Color8 a
          -> Color8 a
rotateHue dh = xterm256ColorToCode . rotateHue' dh . color8CodeToXterm256

{- Returns how many different colors can be obtained from this color by calling 'rotateHue' on it.

To get each one of these colors:

@
  let n = countHuesOfSameIntensity color
      angleIncrement = 1 / fromIntegral n
  in map
      (\i -> rotateHue (fromIntegral i * angleIncrement) color)
      [0..pred n]
@
-}
countHuesOfSameIntensity :: Color8 a -> Int
countHuesOfSameIntensity = countHuesOfSameIntensity' . color8CodeToXterm256

-- | Returns all 'Color8' that have the same /gray/ component as the input 'Color8'
-- and saturated components of the same intensity as the saturated component of the
-- input 'Color8'.
sameIntensityHues :: Color8 a -> [Color8 a]
sameIntensityHues color =
  let n = countHuesOfSameIntensity color
      angleIncrement = 1 / fromIntegral n
  in map
      (\i -> rotateHue (fromIntegral i * angleIncrement) color)
      [0..pred n]
