{-# LANGUAGE NoImplicitPrelude #-}

-- | By convention, discrete positions are converted to continuous positions by
-- placing them at the "pixel center", ie by applying an offset of (0.5, 0.5) in
-- 'pos2vec'.
--
-- Then during the inverse transformation, in 'vec2coords', coordinates are just
-- floored.

module Imj.Geo.Conversion
           ( vec2coords
           , pos2vec
           , speed2vec
           ) where

import           Imj.Prelude

import           Imj.Geo.Continuous.Types
import           Imj.Geo.Discrete.Types

pos2vec :: Coords -> Vec2
pos2vec (Coords r c) = Vec2 (0.5 + fromIntegral c) (0.5 + fromIntegral r)

speed2vec :: Coords -> Vec2
speed2vec (Coords r c) = Vec2 (fromIntegral c) (fromIntegral r)

vec2coords :: Vec2 -> Coords
vec2coords (Vec2 x y) = Coords (floor y) (floor x)
