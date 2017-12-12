
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Geo.Discrete.Types
    ( Col(..)
    , Row(..)
    , Coords(..)
    , PosSpeed(..)
    , Segment(..)
    , sumCoords
    , module Geo.Types
    ) where

import           Imajuscule.Prelude

import           Geo.Types

newtype Row = Row { _rowIndex :: Int } deriving (Eq, Show, Ord, Num, Integral, Enum, Real)
newtype Col = Col { _colIndex :: Int } deriving (Eq, Show, Ord, Num, Integral, Enum, Real)

data Coords = Coords {
    _coordsY :: !Row
  , _coordsX :: !Col
} deriving (Eq, Show, Ord)

sumCoords :: Coords -> Coords -> Coords
sumCoords (Coords (Row r1) (Col c1)) (Coords (Row r2) (Col c2)) =
  Coords (Row $ r1 + r2) (Col $ c1 + c2)

data Segment = Horizontal Row Int Int
             | Vertical   Col Int Int
             | Oblique    Coords Coords


data PosSpeed = PosSpeed {
    _pos :: !Coords
  , _speed :: !Coords
} deriving (Eq, Show)
