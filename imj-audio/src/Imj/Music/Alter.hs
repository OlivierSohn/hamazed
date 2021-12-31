{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Alter
      (
      -- * Alter Instruction
        transposeSymbol
      -- * Combine [Instruction]
      , interleaveSymbols
      , slowDown
      -- * Alter Voice
      , mapVoice
      -- * Alter Score
      , intersperse
      , intercalate
      , transpose
      , mute
      ) where

import           Imj.Prelude
import qualified Data.Vector as V

import qualified Data.List as List
import           Imj.Music.Instruction
import           Imj.Music.Score

-- | Returns a list of the same size as the input, with only 'Rest' elements in it.
mute :: [Instruction] -> [Instruction]
mute l = List.replicate (List.length l) Rest

slowDown :: Int -> [Instruction] -> [Instruction]
slowDown factor is = go is []
 where
  go [] res = reverse res
  go (r:remaining) res =
    go remaining (List.take (factor - 1) (repeat extension) ++ (r:res))
   where
    extension = case r of
      Note _ _ -> Extend
      Extend -> Extend
      Rest -> Rest

-- | Interleaves the symbols of the two lists
interleaveSymbols :: [Instruction] -> [Instruction] -> [Instruction]
interleaveSymbols v1 v2 = concatMap (\(a,b) -> [a,b]) $ zip v1 v2

intercalateSymbols :: [Instruction] -> [Instruction] -> [Instruction]
intercalateSymbols s i = concatMap (\sy -> [sy] ++ i) s

mapVoice :: ([Instruction] -> [Instruction]) -> Voice i -> Voice i
mapVoice f v = v { voiceInstructions = V.fromList $ f $ V.toList $ voiceInstructions v }

transposeSymbol :: Int
                -- ^ Count of semitones
                -> Instruction
                -> Instruction
transposeSymbol s (Note n o) = uncurry Note $ midiPitchToNoteAndOctave (fromIntegral s + noteToMidiPitch n o)
transposeSymbol _ Rest   = Rest
transposeSymbol _ Extend = Extend

intersperse :: Instruction -> Score i -> Score i
intersperse s (Score voices) =
  Score $
    map
      (mapVoice $ (\v ->
        interleaveSymbols v (repeat s)
        )) voices

intercalate :: [Instruction] -> Score i -> Score i
intercalate symbols (Score voices) =
  Score $
    map
      (mapVoice $ (\v ->
        intercalateSymbols v symbols
        )) voices

-- | Transposes a 'Score' by a given number of semitones.
transpose :: Int -> Score i -> Score i
transpose n (Score voices) =
  Score $
    map
      (mapVoice $ map $ transposeSymbol n)
      voices
