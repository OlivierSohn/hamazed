
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World.Space.Read
    ( readWorld
    , writeWorld
    ) where

import           Imj.Prelude

import           Data.List(length)

import           Imj.Game.Hamazed.World.Space.Types

import           Imj.Data.Matrix.Cyclic


readWorld :: [String] -> Matrix Material
readWorld [] = fromList 0 0 []
readWorld l@(s:_)
  | any (/= len) lens = error $ "lengths should all be equal:" ++ show lens
  | otherwise = fromLists $ map (map toMaterial) l
  where
   len = length s
   lens = map length l

writeWorld :: Matrix Material -> [String]
writeWorld = map (map toChar) . toLists

toMaterial :: Char -> Material
toMaterial 'O' = Air
toMaterial ' ' = Wall
toMaterial x = error $ "Can't parse '" ++ show x ++ "' as a Material"

-- unlike in the game, we draw Air to better see the component shapes
toChar :: Material -> Char
toChar Air = 'O'
toChar Wall = ' '
