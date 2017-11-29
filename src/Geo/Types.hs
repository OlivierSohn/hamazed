
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Geo.Types
    ( Direction(..)
    , Col(..)
    , Row(..)
    , Coords(..)
    , PosSpeed(..)
    , Segment(..)
    , Vec2(..)
    ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )

data Direction = Up | Down | LEFT | RIGHT deriving (Eq, Show)

-----------------------------------------------------------------------
-- Discrete geometry
-----------------------------------------------------------------------

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

-----------------------------------------------------------------------
-- Continuous geometry
-----------------------------------------------------------------------

data Vec2 = Vec2 Float Float deriving(Generic, Eq, Show)
