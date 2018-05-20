{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Alter
      (
      -- * alter Symbol
        transposeSymbol
      -- * combine [Symbol]
      , mixSymbols
      -- * alter Voice
      , mapVoice
      -- * alter Score
      , intersperse
      , transpose
      ) where

import           Imj.Prelude
import qualified Data.Vector as V

import           Imj.Music.Types

mixSymbols :: [Symbol] -> [Symbol] -> [Symbol]
mixSymbols v1 v2 = concatMap (\(a,b) -> [a,b]) $ zip v1 v2

mapVoice :: ([Symbol] -> [Symbol]) -> Voice -> Voice
mapVoice f v = v { voiceSymbols = V.fromList $ f $ V.toList $ voiceSymbols v }

transposeSymbol :: Int
                -- ^ Count of semi-tones
                -> Symbol -> Symbol
transposeSymbol n (Note s) = Note $ toEnum $ n + fromEnum s
transposeSymbol _ Rest   = Rest
transposeSymbol _ Extend = Extend

intersperse :: Symbol -> Score -> Score
intersperse s (Score voices) =
  Score $
    map
      (mapVoice $ (\v ->
        mixSymbols v (repeat s)
        )) voices

transpose :: Int -> Score -> Score
transpose n (Score voices) =
  Score $
    map
      (mapVoice $ map $ transposeSymbol n)
      voices
