{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Geo.Discrete
           ( module Imj.Geo.Discrete.Types
           -- * Construct Segment
           , changeSegmentCount
           -- * Use Segment
           , extremities
           -- * Use Coords
           , diffPosToSpeed
           , sumPosSpeed
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
import qualified Prelude as Unsafe(last)

import           Data.Word(Word32)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Positionable

import           Imj.Geo.Discrete.Bresenham
import           Imj.Geo.Discrete.Bresenham3
import           Imj.Geo.Discrete.Resample

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


-- | Modifies the number of elements in a 'Segment'
changeSegmentCount :: Int -> Segment -> Segment
changeSegmentCount n'
  | n' <= 0 = error $ "Segment count should be strictly positive : " ++ show n'
  | otherwise = \case
      (Horizontal row c1 _) -> Horizontal row c1 $ c1 + fromIntegral n
      (Vertical   col r1 _) -> Vertical   col r1 $ r1 + fromIntegral n
      (Oblique c1 c2 _) -> Oblique c1 c2 $ fromIntegral n'
    where
      n = pred n'

-- | Returns the start and end coordinates.
extremities :: Segment -> (Coords Pos, Coords Pos)
extremities (Horizontal row c1 c2) = (Coords row c1, Coords row c2)
extremities (Vertical   col r1 r2) = (Coords r1 col, Coords r2 col)
extremities oblique@(Oblique c1 _ l)
  | l <= 0 = error $ "Segment length should be strictly positive : " ++ show l
  | otherwise = (c1, Unsafe.last $ bresenham oblique)

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
