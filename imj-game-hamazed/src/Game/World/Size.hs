{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Size
    ( worldSizeFromLevel
    , maxWorldSize
    , onFronteer
    , contains
    ) where

import           Imajuscule.Prelude

import           Geo.Discrete.Types

import           Game.Level.Types
import           Game.World.Types

mkWorldSize :: Height -> Width -> WorldSize
mkWorldSize (Height r) (Width c) = WorldSize $ Coords (Coord r) (Coord c)

maxLevelHeight :: Int
maxLevelHeight = 36

maxLevelWidth :: Int
maxLevelWidth = 2 * maxLevelHeight

maxWorldSize :: WorldSize
maxWorldSize = mkWorldSize (Height maxLevelHeight) (Width maxLevelWidth)

worldSizeFromLevel :: Int -> WorldShape -> WorldSize
worldSizeFromLevel level shape =
  let s = maxLevelHeight + 2 * (firstLevel-level) -- less and less space as level increases
      -- we need even world dimensions to ease level construction
      width = assert (even s) s * case shape of
        Square       -> 1
        Rectangle2x1 -> 2
  in mkWorldSize (Height s) (Width width)

onFronteer :: Coords -> WorldSize -> Maybe Direction
onFronteer (Coords r c) (WorldSize (Coords rs cs))
  | r == -1 = Just Up
  | c == -1 = Just LEFT
  | r == rs = Just Down
  | c == cs = Just RIGHT
  | otherwise = Nothing

contains :: Coords -> WorldSize -> Bool
contains (Coords r c) (WorldSize (Coords rs cs))
  = r >= -1 && c >= -1 && r <= rs && c <= cs
