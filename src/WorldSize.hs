
module WorldSize
    ( WorldSize(..)
    , Location(..)
    ) where

import           Imajuscule.Prelude

import           Geo( Coords(..) )

newtype WorldSize = WorldSize Coords

data Location = InsideWorld
              | OutsideWorld
              deriving(Eq, Show)
