{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Random.Test
       ( splitRandWord32
       , splitRandWord32L
       ) where

import           Imj.Prelude
import           Data.Word(Word32)

-- | Generates random 'Word32' using the supplied action, and counts how many are below the threshold.
splitRandWord32 :: Int -> Word32 -> IO Word32 -> IO Int
splitRandWord32 count threshold gen =
  go 0 count
 where
  go s 0 = return s
  go s i = gen >>= \r -> do
    let s' = bool s (s+1) $ r < threshold
    go s' (i-1)

splitRandWord32L :: Int -> Word32 -> (Int -> IO [Word32]) -> IO Int
splitRandWord32L count threshold gen =
  go 0 count <$> gen count
 where
  go s 0 _ = s
  go _ _ [] = error "logic"
  go s i (r:rs) =
    let s' = bool s (s+1) $ r < threshold
    in go s' (i-1) rs
