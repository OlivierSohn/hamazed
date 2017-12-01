{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Size
    ( worldSizeFromLevel
    , maxWorldSize
    , maxDim
    , onFronteer
    , contains
    , showUpdateTick
    ) where

import           Imajuscule.Prelude

import           Data.Text( Text, pack )

import           Geo.Discrete.Types

import           Game.Level.Types
import           Game.World.Types

mkWorldSize :: Height -> Width -> WorldSize
mkWorldSize (Height r) (Width c) = WorldSize $ Coords (Row r) (Col c)

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
onFronteer (Coords (Row r) (Col c)) (WorldSize (Coords (Row rs) (Col cs)))
  | r == -1 = Just Up
  | c == -1 = Just LEFT
  | r == rs = Just Down
  | c == cs = Just RIGHT
  | otherwise = Nothing

contains :: Coords -> WorldSize -> Bool
contains (Coords (Row r) (Col c)) (WorldSize (Coords (Row rs) (Col cs)))
  = r >= -1 && c >= -1 && r <= rs && c <= cs

maxDim :: WorldSize -> Int
maxDim (WorldSize (Coords (Row rs) (Col cs))) = max rs cs


showUpdateTick :: Int -> WorldSize -> Text
showUpdateTick t (WorldSize (Coords _ c@(Col cs))) =
  let l = tickRepresentationLength c
      nDotsBefore = max 0 (t + l - cs)
      nLeftBlanks = t - nDotsBefore
      nDotsAfter = l - nDotsBefore
      nRightBlanks = cs - t - l
  in pack $ replicate nDotsBefore  '.'
  ++ replicate nLeftBlanks  ' '
  ++ replicate nDotsAfter   '.'
  ++ replicate nRightBlanks ' '

tickRepresentationLength :: Col -> Int
tickRepresentationLength (Col c) = quot c 2
