{-# LANGUAGE NoImplicitPrelude #-}

module WorldSize
    ( WorldShape(..)
    , WorldSize(..)
    , worldSizeFromLevel
    , Location(..)
    ) where

import           Imajuscule.Prelude

import           Geo( Coords(..)
                    , Row(..)
                    , Col(..) )

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

worldSizeFromLevel :: Int -> WorldShape -> WorldSize
worldSizeFromLevel level shape =
  let s = 36 + 2 * (1-level) -- less and less space as level increases
      -- we need even world dimensions to ease level construction
      width = assert (even s) s * case shape of
        Square       -> 1
        Rectangle2x1 -> 2
  in mkWorldSize (Height s) (Width width)
