{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types for discrete geometry.

module Imj.Geo.Discrete.Types
    (
    -- * 2D coordinates
      Col
    , Row
    , Coords(..)
    , Coord(..)
    -- * 2D size
    , Size(..)
    , Length(..)
    , Width
    , Height
    , toCoords
    , maxDim
    , onFronteer
    , contains
    -- * 2D segment
    , Segment(..)
    -- * Utilities
    , sumCoords
    -- * Reexports
    , module Imj.Geo.Types
    ) where

import           Imj.Prelude

import           Imj.Geo.Types

-- | Discrete coordinate.
newtype Coord a = Coord Int
  deriving (Eq, Num, Ord, Integral, Real, Enum, Show)

-- | Represents a row index (y)
data Row
-- | Represents a column index (x)
data Col

-- | Two-dimensional discrete coordinates.
--
-- Note that it has a 'DiscretelyInterpolable' instance which is not shown in the
--  documentation because it is defined in module "imj-animation-base".
data Coords = Coords {
    _coordsY :: {-# UNPACK #-} !(Coord Row)
  , _coordsX :: {-# UNPACK #-} !(Coord Col)
} deriving (Eq, Show, Ord)

sumCoords :: Coords -> Coords -> Coords
sumCoords (Coords r1 c1) (Coords r2 c2) =
  Coords (r1 + r2) (c1 + c2)

-- | Discrete length
newtype Length a = Length Int
  deriving (Eq, Num, Ord, Integral, Real, Enum, Show)

data Width
data Height
data Size = Size {
    _sizeY :: {-# UNPACK #-} !(Length Height)
  , _sizeX :: {-# UNPACK #-} !(Length Width)
} deriving (Eq, Show)

toCoords :: Length Height -> Length Width -> Coords
toCoords (Length h) (Length w) =
  Coords (Coord h) (Coord w)

maxDim :: Size -> Int
maxDim (Size rs cs) = max (fromIntegral rs) (fromIntegral cs)

onFronteer :: Coords -> Size -> Maybe Direction
onFronteer (Coords r c) (Size rs cs)
  | r == -1 = Just Up
  | c == -1 = Just LEFT
  | r == fromIntegral rs = Just Down
  | c == fromIntegral cs = Just RIGHT
  | otherwise = Nothing

contains :: Coords -> Size -> Bool
contains (Coords r c) (Size rs cs)
  = r >= -1 && c >= -1 && r <= fromIntegral rs && c <= fromIntegral cs

data Segment = Horizontal !(Coord Row) !(Coord Col) !(Coord Col)
             | Vertical   !(Coord Col) !(Coord Row) !(Coord Row)
             | Oblique    !Coords !Coords
