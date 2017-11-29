{-# LANGUAGE NoImplicitPrelude #-}

module WorldSize
    ( WorldShape(..)
    , WorldSize(..)
    , worldSizeFromLevel
    , maxWorldSize
    , Location(..)
    , onFronteer
    , contains
    ) where

import           Imajuscule.Prelude

import           Geo.Discrete.Types

data WorldShape = Square
                | Rectangle2x1

newtype WorldSize = WorldSize Coords

newtype Width = Width Int
newtype Height = Height Int

data Location = InsideWorld
              | OutsideWorld
              deriving(Eq, Show)

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
  let s = maxLevelHeight + 2 * (1-level) -- less and less space as level increases
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
