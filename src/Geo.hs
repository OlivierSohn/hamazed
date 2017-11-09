{-# LANGUAGE DeriveGeneric #-}

module Geo ( Direction(..)
           , Col(..)
           , Coords(..)
           , coordsForDirection
           , Row(..)
           , sumCoords
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
