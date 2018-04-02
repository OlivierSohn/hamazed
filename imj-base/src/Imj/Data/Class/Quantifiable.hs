{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Data.Class.Quantifiable
    ( Quantifiable(..)
    ) where

import           Imj.Prelude

import           Data.List(length, foldl')

import           Imj.Util

class (Ord a, Show a) => Quantifiable a where
  readFloat :: Float -> a
  writeFloat :: a -> Float

  -- | Can be seen as a zero, and is the result of 'partition 0 x'.
  nothing :: a
  nothing = readFloat 0

  gather :: a -> a -> a
  gather a b = readFloat $ writeFloat a + writeFloat b

  scatter :: Int -> a -> a
  scatter 0 _ = nothing
  scatter n a = readFloat $ writeFloat a / fromIntegral n

  average :: [a] -> a
  average l = scatter (length l) $ foldl' gather nothing l

  normalize :: [a] -> [Float]
  normalize l =
    let fs = map writeFloat l
        maxQty = fromMaybe (error "logic") $ maximumMaybe fs
    in  map (/maxQty) fs

  showQty :: a -> String
  showQty = show

instance Quantifiable Float where
  readFloat = realToFrac
  writeFloat = realToFrac
