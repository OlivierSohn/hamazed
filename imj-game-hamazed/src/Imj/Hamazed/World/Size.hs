{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Hamazed.World.Size
    ( maxWorldSize
    , worldSizeFromLevel
    ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types
import           Imj.Hamazed.Level.Types
import           Imj.Hamazed.World.Types

maxLevelHeight :: Length Height
maxLevelHeight = 36

maxLevelWidth :: Length Width
maxLevelWidth = 2 * fromIntegral maxLevelHeight

maxWorldSize :: Size
maxWorldSize = Size maxLevelHeight maxLevelWidth

heightFromLevel :: Int -> Length Height
heightFromLevel level =
  maxLevelHeight + fromIntegral (2 * (firstLevel-level)) -- less and less space as level increases


worldSizeFromLevel :: Int
                   -- ^ 'Level' number
                   -> WorldShape -> Size
worldSizeFromLevel level shape =
  let h = heightFromLevel level
      -- we need even world dimensions to ease level construction
      w = fromIntegral $ assert (even h) h * case shape of
        Square       -> 1
        Rectangle2x1 -> 2
  in Size h w
