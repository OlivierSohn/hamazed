{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types for discrete geometry.

module Imj.Geo.Discrete.Types
    (
    -- * Discrete geometry types
    -- ** Direction
      Direction(..)
    -- ** Coordinates
    , Col
    , Row
    , Coords(..)
    , Coord(..)
    -- ** Size
    , Size(..)
    , Length(..)
    , Width
    , Height
    , toCoords
    , maxLength
    , onOuterBorder
    , containsWithOuterBorder
    -- ** Segment
    , Segment(..)
    ) where

import           Imj.Prelude

-- | Discrete directions.
data Direction = Up | Down | LEFT | RIGHT deriving (Eq, Show)

-- | Discrete coordinate.
newtype Coord a = Coord Int
  deriving (Eq, Num, Ord, Integral, Real, Enum, Show)

-- | Represents a row index (y)
data Row
-- | Represents a column index (x)
data Col

-- | Two-dimensional discrete coordinates.
data Coords = Coords {
    _coordsY :: {-# UNPACK #-} !(Coord Row)
  , _coordsX :: {-# UNPACK #-} !(Coord Col)
} deriving (Eq, Show, Ord)

-- | Discrete length
newtype Length a = Length Int
  deriving (Eq, Num, Ord, Integral, Real, Enum, Show)

-- | Phantom type for width
data Width
-- | Phantom type for height
data Height
-- | Represents a discrete size (width and height)
data Size = Size {
    _sizeY :: {-# UNPACK #-} !(Length Height)
  , _sizeX :: {-# UNPACK #-} !(Length Width)
} deriving (Eq, Show)

-- | Width and Height to Coords
toCoords :: Length Height -> Length Width -> Coords
toCoords (Length h) (Length w) =
  Coords (Coord h) (Coord w)

-- | Returns the bigger dimension (width or height)
maxLength :: Size -> Int
maxLength (Size (Length h) (Length w)) =
  max w h

-- | Tests if a 'Coords' lies on the outer border of a region of a given size,
-- containing (0,0) and positive coordinates.
onOuterBorder :: Coords
              -- ^ The coordinates to test
              -> Size
              -- ^ The size
              -> Maybe Direction
              -- ^ If the coordinates are on the border, returns a 'Direction' pointing
              -- away from the region (at the given coordinates).
onOuterBorder (Coords r c) (Size rs cs)
  | r == -1 = Just Up
  | c == -1 = Just LEFT
  | r == fromIntegral rs = Just Down
  | c == fromIntegral cs = Just RIGHT
  | otherwise = Nothing

-- | Tests if a 'Coords' is contained or on the outer border of a region
-- of a given size, containing (0,0) and positive coordinates.
containsWithOuterBorder :: Coords -> Size -> Bool -- TODO simplify, pass a number for the outer border size
containsWithOuterBorder (Coords r c) (Size rs cs)
  = r >= -1 && c >= -1 && r <= fromIntegral rs && c <= fromIntegral cs

-- | A segment is a line betwen two discrete coordinates.
--
-- It can be materialized as a list of 'Coords' using 'bresenham'
data Segment = Horizontal !(Coord Row) !(Coord Col) !(Coord Col)
             -- ^ Horizontal segment
             | Vertical   !(Coord Col) !(Coord Row) !(Coord Row)
             -- ^ Vertical segment
             | Oblique    !Coords !Coords
             -- ^ Oblique segment
             deriving(Show)
