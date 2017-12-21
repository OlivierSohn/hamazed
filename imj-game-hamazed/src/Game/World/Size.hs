{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Size
    ( worldSizeFromLevel
    , maxWorldSize
    ) where

import           Imajuscule.Prelude

import           Geo.Discrete.Types

import           Game.Level.Types
import           Game.World.Types

maxLevelHeight :: Length Height
maxLevelHeight = 36

maxLevelWidth :: Length Width
maxLevelWidth = 2 * fromIntegral maxLevelHeight

maxWorldSize :: Size
maxWorldSize = Size maxLevelHeight maxLevelWidth

worldSizeFromLevel :: Int -> WorldShape -> Size
worldSizeFromLevel level shape =
  let height = maxLevelHeight + fromIntegral (2 * (firstLevel-level)) -- less and less space as level increases
      -- we need even world dimensions to ease level construction
      width = fromIntegral $ assert (even height) height * case shape of
        Square       -> 1
        Rectangle2x1 -> 2
  in Size height width
