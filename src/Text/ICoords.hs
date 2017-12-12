
{-# LANGUAGE NoImplicitPrelude #-}


module Text.ICoords
    ( ICoords(..)
    ) where

import           Imajuscule.Prelude

import           Geo.Discrete.Types

import           Interpolation

import           Math

newtype ICoords = ICoords Coords deriving(Eq, Show)
-- | Move horizontaly then vertically (an arbitrary choice that can be discussed)
instance DiscretelyInterpolable ICoords where
  distance (ICoords (Coords (Row r) (Col c))) (ICoords (Coords (Row r') (Col c'))) =
    1 + abs (r-r') + abs (c-c')
  interpolate (ICoords from@(Coords (Row r) (Col c))) (ICoords( Coords (Row r') (Col c'))) progress =
    let v = signum (r'-r)
        h = signum (c'-c)
        maxCountV = abs (r'-r)
        maxCountH = abs (c'-c)
        countH = clamp progress 0 maxCountH
        tmp = max 0 (progress - countH)
        countV = min maxCountV (assert (tmp <= maxCountV) tmp)
        dc = Coords (Row (v * countV)) (Col (h * countH))
    in ICoords $ sumCoords from dc
