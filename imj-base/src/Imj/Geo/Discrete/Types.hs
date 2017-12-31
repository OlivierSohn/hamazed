{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Types for discrete geometry.

module Imj.Geo.Discrete.Types
    (
    -- * Discrete geometry types
    -- ** Direction
      Direction(..)
    -- ** Coordinates
    , Coords(..)
    , Coord(..), Col, Row
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
    , mkSegment
    -- * Bresenham line algorithm
    , bresenhamLength
    , bresenham
    -- * Reexports
    , Pos, Vel
    ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Bresenham
import           Imj.Geo.Types
import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Util

-- | Discrete directions.
data Direction = Up | Down | LEFT | RIGHT deriving (Eq, Show)

-- | Discrete coordinate.
newtype Coord a = Coord Int
  deriving (Eq, Num, Ord, Integral, Real, Enum, Show)

-- | Using bresenham 2d line algorithm.
instance DiscreteInterpolation (Coords Pos) where
  interpolate c c' i
    | c == c' = c
    | otherwise =
        let lastFrame = pred $ fromIntegral $ bresenhamLength c c'
            -- TODO measure if "head . drop (pred n)"" is more optimal than "!! n"
            index = clamp i 0 lastFrame
        in head . drop index $ bresenham $ mkSegment c c'

-- | Using bresenham 2d line algorithm.
instance DiscreteDistance (Coords Pos) where
  distance = bresenhamLength

-- | Represents a row index (y)
data Row
-- | Represents a column index (x)
data Col

-- | Two-dimensional discrete coordinates. We use phantom types 'Pos', 'Vel'
-- to distinguish positions from speeds.
data Coords a = Coords {
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
toCoords :: Length Height -> Length Width -> Coords Pos
toCoords (Length h) (Length w) =
  Coords (Coord h) (Coord w)

-- | Returns the bigger dimension (width or height)
maxLength :: Size -> Int
maxLength (Size (Length h) (Length w)) =
  max w h

-- | Tests if a 'Coords' lies on the outer border of a region of a given size,
-- containing (0,0) and positive coordinates.
onOuterBorder :: Coords Pos
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
containsWithOuterBorder :: Coords Pos -> Size -> Bool -- TODO simplify, pass a number for the outer border size
containsWithOuterBorder (Coords r c) (Size rs cs)
  = r >= -1 && c >= -1 && r <= fromIntegral rs && c <= fromIntegral cs

-- | A segment is a line betwen two discrete coordinates.
--
-- It can be materialized as a list of 'Coords' using 'bresenham'
data Segment = Horizontal !(Coord Row) !(Coord Col) !(Coord Col)
             -- ^ Horizontal segment
             | Vertical   !(Coord Col) !(Coord Row) !(Coord Row)
             -- ^ Vertical segment
             | Oblique    !(Coords Pos) !(Coords Pos)
             -- ^ Oblique segment
             deriving(Show)

mkSegment :: Coords Pos
          -- ^ Segment start
          -> Coords Pos
          -- ^ Segment end
          -> Segment
mkSegment coord1@(Coords r1 c1) coord2@(Coords r2 c2)
  | r1 == r2  = Horizontal r1 c1 c2
  | c1 == c2  = Vertical   c1 r1 r2
  | otherwise = Oblique coord1 coord2


-- | Returns the bresenham 2d distance between two coordinates.
bresenhamLength :: Coords Pos -> Coords Pos -> Int
bresenhamLength (Coords r1 c1) (Coords r2 c2)
  = succ $ max (fromIntegral (abs (r1-r2))) $ fromIntegral (abs (c1-c2))

-- | Bresenham 2d algorithm, slightly optimized for horizontal and vertical lines.
bresenham :: Segment -> [Coords Pos]
bresenham (Horizontal r c1 c2) = map (Coords r) $ range c1 c2
bresenham (Vertical c r1 r2)   = map (flip Coords c) $ range r1 r2
bresenham (Oblique (Coords y0 x0) c2@(Coords y1 x1)) =
  takeWhileInclusive (/= c2)
  $ map (\(x,y) -> Coords (Coord y) (Coord x) )
  $ bla (fromIntegral x0,fromIntegral y0)
        (fromIntegral x1,fromIntegral y1)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) =
  x : if p x
        then
          takeWhileInclusive p xs
        else
          []
