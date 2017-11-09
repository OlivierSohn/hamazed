{-# LANGUAGE DeriveGeneric #-}

module Geo ( Direction(..)
           , Col(..)
           , Coords(..)
           , coordsForDirection
           , Line(..)
           , mkLine
           , Row(..)
           , sumCoords
           , translateCoord
           , zeroCoords ) where


import           Prelude hiding ( Left
                                , Right )
import           GHC.Generics( Generic )


--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data Direction = Up | Down | Left | Right deriving (Eq, Show)

newtype Row = Row { _rowIndex :: Int } deriving (Generic, Eq, Show)
newtype Col = Col { _colIndex :: Int } deriving (Generic, Eq, Show)

data Coords = Coords {
    _x :: !Row
  , _y :: !Col
} deriving (Generic, Eq, Show)


zeroCoords :: Coords
zeroCoords = Coords (Row 0) (Col 0)


sumCoords :: Coords -> Coords -> Coords
sumCoords (Coords (Row r1) (Col c1)) (Coords (Row r2) (Col c2)) = Coords (Row $ r1 + r2) (Col $ c1 + c2)


coordsForDirection :: Direction -> Coords
coordsForDirection Down  = Coords (Row   1) (Col   0)
coordsForDirection Up    = Coords (Row$ -1) (Col   0)
coordsForDirection Left  = Coords (Row   0) (Col$ -1)
coordsForDirection Right = Coords (Row   0) (Col   1)


translateCoord :: Direction -> Coords -> Coords
translateCoord dir = sumCoords $ coordsForDirection dir


data Line = Horizontal Row Int Int
          | Vertical   Col Int Int
          | Oblique    Coords Coords

mkLine :: Coords -> Coords -> Line
mkLine coord1@(Coords row@(Row r1) col@(Col c1)) coord2@(Coords (Row r2) (Col c2))
  | r1 == r2  = Horizontal row c1 c2
  | c1 == c2  = Vertical   col r1 r2
  | otherwise = Oblique coord1 coord2
