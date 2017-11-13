{-# LANGUAGE DeriveGeneric #-}

module WorldSize
    ( WorldSize(..)
    , location
    , Location(..)
    , rebound
    ) where

import           GHC.Generics( Generic )

import           Geo( Coords(..)
                    , Col(..)
                    , Row(..) )

newtype WorldSize = WorldSize { _worldSizeValue :: Int } deriving(Generic, Eq, Show)

data Location = InsideWorld
              | OutsideWorld
              deriving(Eq, Show)

location :: Coords -> WorldSize -> Location
location (Coords (Row r) (Col c)) (WorldSize worldSize)
  | inside r && inside c = InsideWorld
  | otherwise            = OutsideWorld
  where inside x = x >= 0 && x < worldSize

rebound :: WorldSize -> Coords -> Coords
rebound sz (Coords (Row r) (Col c)) = Coords (Row $ reboundInt sz r) (Col $ reboundInt sz c)

reboundInt :: WorldSize -> Int -> Int
reboundInt s@(WorldSize sz) i
  | i < 0     = reboundInt s $ -i
  | i > sz-1  = reboundInt s $ 2*(sz-1)-i
  | otherwise = i
