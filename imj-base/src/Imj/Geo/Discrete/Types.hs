{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Types for discrete geometry.

module Imj.Geo.Discrete.Types
    (
    -- * Discrete geometry types
    -- ** Direction
      Direction(..)
    -- ** Coordinates
    , Col, Row, Coord(..), Coords(..)
    -- ** Size
    , Size(..)
    , Length(..)
    , Width
    , Height
    , toCoords
    , maxLength
    , area
    -- ** Segment
    , Segment(..)
    , mkSegment
    , countSegmentElements
    -- * Bresenham line algorithm
    , bresenhamLength
    , bresenham
    -- * Reexports
    , Pos, Vel
    ) where

import           Imj.Prelude

import           Control.DeepSeq(NFData)
import           Data.Word(Word32)

import           Imj.Geo.Discrete.Bresenham
import           Imj.Geo.Types
import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Util

-- | Discrete directions.
data Direction = Up | Down | LEFT | RIGHT deriving (Eq, Show, Generic)
instance PrettyVal Direction
instance Binary Direction

-- | One-dimensional discrete coordinate. We use phantom types 'Row', 'Col'
-- to distinguish between rows and columns.
newtype Coord a = Coord Int
  deriving (Eq, Num, Ord, Integral, Real, Enum, Show, Bounded, Binary, NFData, Generic)
instance PrettyVal (Coord a)

-- | Using bresenham 2d line algorithm.
instance DiscreteInterpolation (Coords Pos) where
  interpolate c c' i
    | c == c' = c
    | otherwise =
        let lastFrame = pred $ fromIntegral $ bresenhamLength c c'
        in bresenham (mkSegment c c') !! clamp i 0 lastFrame

-- | Using bresenham 2d line algorithm.
instance DiscreteDistance (Coords Pos) where
  distance a b = fromIntegral $ bresenhamLength a b

{- | Represents a row index (y).

When used to represent a /screen/ row, index 0 is at the /top/ of the screen,
indexes increase towards the /bottom/ of the screen. -}
data Row
{- | Represents a column index (x).

When used to represent a /screen/ column, index 0 is at the /left/ of the screen,
indexes increase towards the /right/ of the screen. -}
data Col

{- | Two-dimensional discrete coordinates. We use phantom types 'Pos', 'Vel'
to distinguish positions from speeds. -}
data Coords a = Coords {
    _coordsY :: {-# UNPACK #-} !(Coord Row)
  , _coordsX :: {-# UNPACK #-} !(Coord Col)
} deriving (Eq, Show, Ord, Generic)
instance NFData (Coords a)
instance Binary (Coords a)
instance PrettyVal (Coords a)

-- | Discrete length
newtype Length a = Length Int
  deriving (Eq, Num, Ord, Integral, Real, Enum, Show, PrettyVal, Generic)

-- | Phantom type for width
data Width
-- | Phantom type for height
data Height
-- | Represents a discrete size (width and height)
data Size = Size {
    getHeight :: {-# UNPACK #-} !(Length Height)
  , getWidth :: {-# UNPACK #-} !(Length Width)
} deriving (Eq, Show, Generic)
instance PrettyVal Size

{-# INLINE area #-}
area :: Size -> Int
area (Size (Length h) (Length w)) = h * w

-- | Width and Height to Coords
{-# INLINE toCoords #-}
toCoords :: Length Height -> Length Width -> Coords Pos
toCoords (Length h) (Length w) =
  Coords (Coord h) (Coord w)

-- | Returns the bigger dimension (width or height)
maxLength :: Size -> Int
maxLength (Size (Length h) (Length w)) =
  max w h

-- | A segment is a line betwen two discrete coordinates.
--
-- It can be materialized as a list of 'Coords' using 'bresenham'
data Segment = Horizontal {-# UNPACK #-} !(Coord Row) {-# UNPACK #-} !(Coord Col) {-# UNPACK #-} !(Coord Col)
             -- ^ Horizontal segment
             | Vertical   {-# UNPACK #-} !(Coord Col) {-# UNPACK #-} !(Coord Row) {-# UNPACK #-} !(Coord Row)
             -- ^ Vertical segment
             | Oblique    {-# UNPACK #-} !(Coords Pos) {-# UNPACK #-} !(Coords Pos) {-# UNPACK #-} !Word32
             -- ^ Oblique segment
             deriving(Generic, Show)

mkSegment :: Coords Pos
          -- ^ Segment start
          -> Coords Pos
          -- ^ Segment end
          -> Segment
mkSegment e1@(Coords r1 c1) e2@(Coords r2 c2)
  | r1 == r2  = Horizontal r1 c1 c2
  | c1 == c2  = Vertical   c1 r1 r2
  | otherwise = Oblique e1 e2 $ bresenhamLength e1 e2

-- | returns the number of elements in a segment
countSegmentElements :: Segment -> Int
countSegmentElements (Horizontal _ c1 c2) = fromIntegral $ succ $ abs $ c2-c1
countSegmentElements (Vertical _ r1 r2)   = fromIntegral $ succ $ abs $ r2-r1
countSegmentElements (Oblique _ _ n)      = fromIntegral n

-- | Returns the bresenham 2d distance between two coordinates.
bresenhamLength :: Coords Pos -> Coords Pos -> Word32
bresenhamLength (Coords r1 c1) (Coords r2 c2)
  = fromIntegral
  $ blaLength (fromIntegral r1, fromIntegral c1)
              (fromIntegral r2, fromIntegral c2)

-- | Bresenham 2d algorithm, slightly optimized for horizontal and vertical lines.
bresenham :: Segment -> [Coords Pos]
bresenham (Horizontal r c1 c2) = map (Coords r) $ range c1 c2
bresenham (Vertical c r1 r2)   = map (flip Coords c) $ range r1 r2
bresenham (Oblique (Coords y0 x0) (Coords y1 x1) l) =
  take (fromIntegral l)
  $ map (\(x,y) -> Coords (Coord y) (Coord x) )
  $ bla (fromIntegral x0,fromIntegral y0)
        (fromIntegral x1,fromIntegral y1)
