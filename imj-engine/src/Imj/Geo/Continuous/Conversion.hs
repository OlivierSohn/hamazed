{-#  OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}


module Imj.Geo.Continuous.Conversion
           ( -- * Conversion to / from discrete coordinates
{- | Discrete positions are converted to continuous positions by
placing them at the "pixel center", ie by applying an offset of (0.5, 0.5) in
'pos2vec'.

Then, during the inverse transformation - in 'vec2coords', coordinates are just
floored.

Discrete speeds are converted with 'speed2vec'. The half-pixel convention is not
applied for speeds. In the future, phantom types will parametrize 'Coords' to
have a robust differentiation between /speed/ coordinates and /position/ coordinates.
-}
             pos2vec
           , vec2coords
           , speed2vec
           ) where

import           Imj.Prelude

import           Imj.Geo.Continuous.Types
import           Imj.Geo.Discrete.Types

-- | Convert a discrete position to a continuous position.
pos2vec :: Coords -> Vec2
pos2vec (Coords r c) = Vec2 (0.5 + fromIntegral c) (0.5 + fromIntegral r)

-- | Convert a discrete speed to a continuous speed.
speed2vec :: Coords -> Vec2
speed2vec (Coords r c) = Vec2 (fromIntegral c) (fromIntegral r)

-- TODO for speeds we should first add .5
-- | Convert a continuous position to a discrete position.
vec2coords :: Vec2 -> Coords
vec2coords (Vec2 x y) = Coords (floor y) (floor x)
