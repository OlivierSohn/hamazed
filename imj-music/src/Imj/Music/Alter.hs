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
      , intercalate
      , transpose
      , mute
      ) where

import           Imj.Prelude
import qualified Data.Vector as V

import qualified Data.List as List
import           Imj.Music.Types

mute :: [Symbol] -> [Symbol]
mute l = List.replicate (List.length l) Rest

mixSymbols :: [Symbol] -> [Symbol] -> [Symbol]
mixSymbols v1 v2 = concatMap (\(a,b) -> [a,b]) $ zip v1 v2

intercalateSymbols :: [Symbol] -> [Symbol] -> [Symbol]
intercalateSymbols s i = concatMap (\sy -> [sy] ++ i) s

mapVoice :: ([Symbol] -> [Symbol]) -> Voice -> Voice
mapVoice f v = v { voiceSymbols = V.fromList $ f $ V.toList $ voiceSymbols v }

transposeSymbol :: Int
                -- ^ Count of semi-tones
                -> Symbol
                -> Symbol
transposeSymbol s (Note n o) = uncurry Note $ midiPitchToNoteAndOctave (fromIntegral s + noteToMidiPitch' n o)
transposeSymbol _ Rest   = Rest
transposeSymbol _ Extend = Extend

intersperse :: Symbol -> Score -> Score
intersperse s (Score voices) =
  Score $
    map
      (mapVoice $ (\v ->
        mixSymbols v (repeat s)
        )) voices

intercalate :: [Symbol] -> Score -> Score
intercalate symbols (Score voices) =
  Score $
    map
      (mapVoice $ (\v ->
        intercalateSymbols v symbols
        )) voices

transpose :: Int -> Score -> Score
transpose n (Score voices) =
  Score $
    map
      (mapVoice $ map $ transposeSymbol n)
      voices
