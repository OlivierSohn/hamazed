{-#  OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}


module Imj.Geo.Continuous.Conversion
           ( -- * Conversion to / from discrete coordinates
{- | Discrete positions are converted to continuous positions by
placing them at the "pixel center", ie by applying an offset of (0.5, 0.5) in
'pos2vec'.

Then, during the inverse transformation - in 'vec2pos', coordinates are just
floored.

Discrete speeds are converted with 'speed2vec'. The half-pixel convention is not
applied for speeds. The inverse conversion is 'vec2speed'.
-}
             pos2vec
           , vec2pos
           , speed2vec
           , vec2speed
           ) where

import           Imj.Prelude

import           Imj.Geo.Continuous.Types
import           Imj.Geo.Discrete.Types

-- | Convert a discrete position to a continuous position.
pos2vec :: Coords Pos -> Vec2 Pos
pos2vec (Coords r c) =
  Vec2 (0.5 + fromIntegral c) (0.5 + fromIntegral r)

-- | Convert a continuous position to a discrete position.
vec2pos :: Vec2 Pos -> Coords Pos
vec2pos (Vec2 x y) =
  Coords (floor y) (floor x)

-- | Convert a discrete speed to a continuous speed.
speed2vec :: Coords Vel -> Vec2 Vel
speed2vec (Coords r c) =
  Vec2 (fromIntegral c) (fromIntegral r)

-- | Convert a continuous speed to a discrete speed.
vec2speed :: Vec2 Vel -> Coords Vel
vec2speed (Vec2 x y) =
  Coords (fromIntegral (round y :: Int)) (fromIntegral (round x :: Int))
