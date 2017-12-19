{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types for discrete geometry.

module Geo.Discrete.Types
    ( Col
    , Row
    , Coords(..)
    , Coord(..)
    , Segment(..)
    , sumCoords
    , module Geo.Types
    ) where

import           Imajuscule.Prelude

import           Geo.Types

-- | Discrete coordinates.
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

sumCoords :: Coords -> Coords -> Coords
sumCoords (Coords r1 c1) (Coords r2 c2) =
  Coords (r1 + r2) (c1 + c2)

data Segment = Horizontal !(Coord Row) !(Coord Col) !(Coord Col)
             | Vertical   !(Coord Col) !(Coord Row) !(Coord Row)
             | Oblique    !Coords !Coords
