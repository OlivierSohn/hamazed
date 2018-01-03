{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Discrete
           ( module Imj.Geo.Discrete.Types
           -- * Construct Segment
           , changeSegmentLength
           -- * Use Segment
           , extremities
           -- * Construct Coords
           , zeroCoords
           , coordsForDirection
           -- * Use Coords
           , diffCoords
           , diffPosToSpeed
           , sumCoords
           , sumPosSpeed
           , move
           , translate
           , translate'
           , translateInDir
           , countInDir
           -- * Discrete algorithms
           -- ** Bresenham
           {- | The 2d version, 'bresenham', allows to
           <https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm draw a line on a 2d grid>.

           The 3d version, 'bresenham3', allows to interpolate discrete colors in RGB space.
           -}
           , module Imj.Geo.Discrete.Bresenham
           , module Imj.Geo.Discrete.Bresenham3
           -- ** List resampling
           {- | Typically, 'resampleWithExtremities' will be used on the result of 'bresenham'
           to over-sample the produced line.
           -}
           , module Imj.Geo.Discrete.Resample
          ) where

import           Imj.Prelude

import           Data.Word(Word32)

import           Imj.Geo.Discrete.Types
import           Imj.Geo.Discrete.Bresenham
import           Imj.Geo.Discrete.Bresenham3
import           Imj.Geo.Discrete.Resample

-- | 'zeroCoords' = 'Coords' 0 0
zeroCoords :: Coords a
zeroCoords = Coords 0 0

-- | Returns a - b
diffCoords :: Coords a
           -- ^ a
           -> Coords a
           -- ^ b
           -> Coords a
           -- ^ a - b
diffCoords (Coords r1 c1) (Coords r2 c2) =
  Coords (r1 - r2) (c1 - c2)

-- | Returns a + b
sumCoords :: Coords a
           -- ^ a
           -> Coords a
           -- ^ b
           -> Coords a
           -- ^ a + b
sumCoords (Coords r1 c1) (Coords r2 c2) =
  Coords (r1 + r2) (c1 + c2)

-- | Assumes that we integrate over one game step.
--
-- Returns a + b
sumPosSpeed :: Coords Pos
            -> Coords Vel
            -> Coords Pos
sumPosSpeed (Coords r1 c1) (Coords r2 c2) =
  Coords (r1 + r2) (c1 + c2)

{-# INLINE diffPosToSpeed #-}
diffPosToSpeed :: Coords Pos
               -> Coords Pos
               -> Coords Vel
diffPosToSpeed (Coords r1 c1) (Coords r2 c2) =
  Coords (r1 - r2) (c1 - c2)

-- | Returns the coordinates that correspond to one step in the given direction.
coordsForDirection :: Direction -> Coords a
coordsForDirection Down  = Coords 1 0
coordsForDirection Up    = Coords (-1) 0
coordsForDirection LEFT  = Coords 0 (-1)
coordsForDirection RIGHT = Coords 0 1

multiply :: Int -> Coords a -> Coords a
multiply n (Coords r c) =
  Coords (r * fromIntegral n) (c * fromIntegral n)

-- | Translate of 1 step in a given direction.
translateInDir :: Direction -> Coords a -> Coords a
translateInDir dir =
  translate $ coordsForDirection dir


-- | Modify the end of the segment to reach the given length
changeSegmentLength :: Int -> Segment -> Segment
changeSegmentLength i (Horizontal row c1 _) = Horizontal row c1 $ c1 + fromIntegral i
changeSegmentLength i (Vertical   col r1 _) = Vertical   col r1 $ r1 + fromIntegral i
changeSegmentLength _ _ = error "changeSegmentLength cannot operate on oblique segments" -- TODO use bresenham if it is valid

-- | Returns the start and end coordinates.
extremities :: Segment -> (Coords Pos, Coords Pos)
extremities (Horizontal row c1 c2) = (Coords row c1, Coords row c2)
extremities (Vertical   col r1 r2) = (Coords r1 col, Coords r2 col)
extremities (Oblique c1 c2)        = (c1, c2)

-- | 'translate' = 'sumCoords'
translate :: Coords a -> Coords a -> Coords a
translate = sumCoords

-- | Translate by a given height and width.
translate' :: Length Height
           -- ^ The height to add
           -> Length Width
           -- ^ The width to add
           -> Coords Pos
           -- The initial coordinates
           -> Coords Pos
translate' h w c =
  sumCoords c $ toCoords h w

move :: Int
     -- ^ Take that many steps
     -> Direction
     -- ^ In that direction
     -> Coords a
     -- ^ From these coordinates
     -> Coords a
move t dir c = sumCoords c $ multiply t $ coordsForDirection dir

-- | Returns the number of valid positions when moving in a given direction
-- from a start position. The start position is tested also, hence when this function
-- returns 0, the start is invalid.
countInDir :: Direction -> Coords Pos -> (Coords Pos -> Bool) -> Word32
countInDir dir start continue =
    extend 0
  where
    extend :: Word32 -> Word32
    extend n =
      if continue $ move (fromIntegral n) dir start
        then
          extend (succ n)
        else
          n
