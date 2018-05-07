{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World.Size
    ( worldSizeFromLevel
    , exhaustiveSmallSizes
    ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Level
import           Imj.Game.Hamazed.World.Types
import           Imj.Geo.Discrete.Types
import           Imj.Space
import           Imj.Util

maxLevelHeight :: Length Height
maxLevelHeight = 36

heightFromLevel :: LevelNumber -> Length Height
heightFromLevel level =
  maxLevelHeight + fromIntegral (2 * (firstLevel-level)) -- less and less space as level increases

worldSizeFromLevel :: LevelNumber
                   -- ^ 'Level' number
                   -> WorldShape
                   -> Size
worldSizeFromLevel level shape =
  let h = heightFromLevel level
      -- we need even world dimensions to ease level construction
      w = fromIntegral $ assert (even h) h * case shape of
        Square       -> 1
        Rectangle'2x1 -> 2
  in Size h w

-- | Returns every (canonicalized) small world sizes used in Hamazed game.
exhaustiveSmallSizes :: [Size]
exhaustiveSmallSizes =
  dedup $ map canonicalize $
    concatMap (\s -> map (bigToSmall s) allBlockSizes) bigSizes
 where
  bigSizes = concatMap
    (\l -> map (worldSizeFromLevel l) [Square, Rectangle'2x1])
    [firstLevel..lastLevel]

  allBlockSizes = [minBlockSize..maxBlockSize]
