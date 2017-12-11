
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
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

import           GHC.Generics( Generic )

import           Geo.Types

import           Interpolation

import           Math

newtype Row = Row { _rowIndex :: Int } deriving (Generic, Eq, Show, Ord, Num)
newtype Col = Col { _colIndex :: Int } deriving (Generic, Eq, Show, Ord, Num)

data Coords = Coords {
    _coordsY :: !Row
  , _coordsX :: !Col
} deriving (Generic, Eq, Show, Ord)


-- | Move horizontaly then vertically (an arbitrary choice that can be discussed)
instance DiscretelyInterpolable Coords where
  distance (Coords (Row r) (Col c)) (Coords (Row r') (Col c')) =
    1 + abs (r-r') + abs (c-c')
  interpolate from@(Coords (Row r) (Col c)) (Coords (Row r') (Col c')) progress =
    let v = signum (r'-r)
        h = signum (c'-c)
        maxCountV = abs (r'-r)
        maxCountH = abs (c'-c)
        countH = clamp progress 0 maxCountH
        tmp = max 0 (progress - countH)
        countV = min maxCountV (assert (tmp <= maxCountV) tmp)
        dc = Coords (Row (v * countV)) (Col (h * countH))
    in  sumCoords from dc

sumCoords :: Coords -> Coords -> Coords
sumCoords (Coords (Row r1) (Col c1)) (Coords (Row r2) (Col c2)) = Coords (Row $ r1 + r2) (Col $ c1 + c2)

data Segment = Horizontal Row Int Int
             | Vertical   Col Int Int
             | Oblique    Coords Coords


data PosSpeed = PosSpeed {
    _pos :: !Coords
  , _speed :: !Coords
} deriving (Generic, Eq, Show, Ord)
