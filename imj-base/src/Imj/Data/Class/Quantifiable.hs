{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Data.Class.Quantifiable
    ( Quantifiable(..)
    , logarithmically
    ) where

import           Imj.Prelude
import qualified Prelude as Unsafe(maximum,minimum)
import           Prelude(logBase)

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
  normalize [] = []
  normalize l =
    let fs = map writeFloat l
        maxQty = fromMaybe (error "logic") $ maximumMaybe fs
    in  map (/maxQty) fs

  showQty :: a -> String
  showQty = show

instance Quantifiable Float where
  readFloat = realToFrac
  writeFloat = realToFrac

{- | Adapts range [min max] (where 0 < min < max) to [0 1]
, with logarithmic scaling.
-}
logarithmically :: (Quantifiable a)
                => Float
                -- ^ Base to use for the logarithm : the bigger, the more it will deviate from linear scaling
                -> [a]
                -> [Float]
logarithmically _ [] = []
logarithmically base l'' =
  map (logBase base) l
 where
  l = map (maybe 1 id . mapRange minD maxD 1 base . writeFloat) l'
  l' = map writeFloat l''
  maxD = Unsafe.maximum l'
  minD = Unsafe.minimum l'
