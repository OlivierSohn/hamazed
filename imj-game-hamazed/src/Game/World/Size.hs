{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Size
    ( worldSizeFromLevel
    , maxWorldSize
    ) where

import           Imajuscule.Prelude

import           Geo.Discrete.Types

import           Game.Level.Types
import           Game.World.Types

mkWorldSize :: Length Height -> Length Width -> WorldSize
mkWorldSize h w = WorldSize $ Size h w

maxLevelHeight :: Int
maxLevelHeight = 36

maxLevelWidth :: Int
maxLevelWidth = 2 * maxLevelHeight

maxWorldSize :: WorldSize
maxWorldSize = mkWorldSize (Length maxLevelHeight) (Length maxLevelWidth)

worldSizeFromLevel :: Int -> WorldShape -> WorldSize
worldSizeFromLevel level shape =
  let s = maxLevelHeight + 2 * (firstLevel-level) -- less and less space as level increases
      -- we need even world dimensions to ease level construction
      width = assert (even s) s * case shape of
        Square       -> 1
        Rectangle2x1 -> 2
  in mkWorldSize (Length s) (Length width)
