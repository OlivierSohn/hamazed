{-# LANGUAGE NoImplicitPrelude #-}

module Geo.Conversion
           ( vec2coords
           , pos2vec
           , speed2vec
           ) where

import           Imajuscule.Prelude

import           Geo.Continuous.Types
import           Geo.Discrete.Types


pos2vec :: Coords -> Vec2
pos2vec (Coords r c) = Vec2 (0.5 + fromIntegral c) (0.5 + fromIntegral r)

speed2vec :: Coords -> Vec2
speed2vec (Coords r c) = Vec2 (fromIntegral c) (fromIntegral r)

vec2coords :: Vec2 -> Coords
vec2coords (Vec2 x y) = Coords (floor y) (floor x)
