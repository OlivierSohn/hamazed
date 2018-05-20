{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Alter
      ( mix
      , mapVoice
      ) where

import           Imj.Prelude
import qualified Data.Vector as V

import           Imj.Music.Types

mix :: [Symbol] -> [Symbol] -> [Symbol]
mix v1 v2 = concatMap (\(a,b) -> [a,b]) $ zip v1 v2

mapVoice :: ([Symbol] -> [Symbol]) -> Voice -> Voice
mapVoice f v = v { voiceSymbols = V.fromList $ f $ V.toList $ voiceSymbols v }
