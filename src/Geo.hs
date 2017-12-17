{-# LANGUAGE NoImplicitPrelude #-}

module Geo ( rotateCcw
           -- * Reexports
           , module Geo.Types
           ) where

import           Imajuscule.Prelude

import           Geo.Types


ccwDirections :: Int -> Direction
ccwDirections i = case i `mod` 4 of
                    0 -> Up
                    1 -> LEFT
                    2 -> Down
                    3 -> RIGHT
                    n -> error $ "out of bound modulo : " ++ show n

ccwDirectionsIndex :: Direction -> Int
ccwDirectionsIndex Up = 0
ccwDirectionsIndex LEFT = 1
ccwDirectionsIndex Down = 2
ccwDirectionsIndex RIGHT = 3

rotateCcw :: Int -> Direction -> Direction
rotateCcw n dir = ccwDirections $ n + ccwDirectionsIndex dir
