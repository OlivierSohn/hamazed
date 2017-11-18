
module WorldSize
    ( WorldSize(..)
    , Width(..)
    , Height(..)
    , mkWorldSize
    , Location(..)
    ) where

import           Imajuscule.Prelude

import           Geo( Coords(..)
                    , Row(..)
                    , Col(..) )

newtype WorldSize = WorldSize Coords

newtype Width = Width Int
newtype Height = Height Int

data Location = InsideWorld
              | OutsideWorld
              deriving(Eq, Show)

mkWorldSize :: Height -> Width -> WorldSize
mkWorldSize (Height r) (Width c) = WorldSize $ Coords (Row r) (Col c)
