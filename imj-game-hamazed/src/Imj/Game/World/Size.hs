{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.World.Size
    ( maxWorldSize
    , heightFromLevel
    ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types

import           Imj.Game.Level.Types

maxLevelHeight :: Length Height
maxLevelHeight = 36

maxLevelWidth :: Length Width
maxLevelWidth = 2 * fromIntegral maxLevelHeight

maxWorldSize :: Size
maxWorldSize = Size maxLevelHeight maxLevelWidth

heightFromLevel :: Int -> Length Height
heightFromLevel level =
  maxLevelHeight + fromIntegral (2 * (firstLevel-level)) -- less and less space as level increases
