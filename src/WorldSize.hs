{-# LANGUAGE DeriveGeneric #-}

module WorldSize
    ( WorldSize(..)
      , location
      , Location(..)
    ) where

import           GHC.Generics( Generic )

import           Geo( Coords(..)
                    , Col(..)
                    , Row(..) )

newtype WorldSize = WorldSize { _worldSizeValue :: Int } deriving(Generic, Eq, Show)

data Location = InsideWorld | OutsideWorld

location :: Coords -> WorldSize -> Location
location (Coords (Row r) (Col c)) (WorldSize worldSize)
  | inside r && inside c = InsideWorld
  | otherwise            = OutsideWorld
  where inside x = x >= 0 && x < worldSize
