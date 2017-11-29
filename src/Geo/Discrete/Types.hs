
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Geo.Discrete.Types
    ( Col(..)
    , Row(..)
    , Coords(..)
    , PosSpeed(..)
    , Segment(..)
    , module Geo.Types
    ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )

import           Geo.Types


newtype Row = Row { _rowIndex :: Int } deriving (Generic, Eq, Show, Ord)
newtype Col = Col { _colIndex :: Int } deriving (Generic, Eq, Show, Ord)

data Coords = Coords {
    _x :: !Row
  , _y :: !Col
} deriving (Generic, Eq, Show, Ord)

data Segment = Horizontal Row Int Int
             | Vertical   Col Int Int
             | Oblique    Coords Coords


data PosSpeed = PosSpeed {
    _pos :: !Coords
  , _speed :: !Coords
} deriving (Generic, Eq, Show, Ord)
