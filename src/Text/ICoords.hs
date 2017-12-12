
{-# LANGUAGE NoImplicitPrelude #-}


module Text.ICoords
    ( ICoords(..)
    ) where

import           Imajuscule.Prelude

import           Geo.Discrete.Types

import           Interpolation

newtype ICoords = ICoords Coords deriving(Eq, Show)
-- | Move horizontaly then vertically (an arbitrary choice that can be discussed)
instance DiscretelyInterpolable ICoords where
  distance (ICoords (Coords r c)) (ICoords (Coords r' c')) =
    1 + fromIntegral (abs (r-r')) + fromIntegral (abs (c-c'))
  interpolate (ICoords from@(Coords r c)) (ICoords (Coords r' c')) progress =
    let v = signum (r'-r)
        h = signum (c'-c)
        maxCountV = abs (r'-r)
        maxCountH = abs (c'-c)
        countH = min (assert (progress >= 0) $ fromIntegral progress) maxCountH
        tmp = fromIntegral $ max 0 (fromIntegral progress - countH)
        countV = min maxCountV (assert (tmp <= fromIntegral maxCountV) tmp)
        dc = Coords (v * countV) (h * countH)
    in ICoords $ sumCoords from dc
