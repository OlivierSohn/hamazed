
module Imj.Music.Harmony
      ( -- | Standard scales
        majorScale
      , minorNaturalScale
      , minorMelodicScale
      , minorHarmonicScale
      -- | Chords
      , majorChord
      , minorChord
      -- | Utilities
      , inPattern
      , NotesPattern(..)
      ) where

import Imj.Music.Instruction(MidiPitch(..))

data NotesPattern = NotesPattern {
    nodesPatternList :: [Int]
  , nodesPatternRoot :: {-# UNPACK #-} !MidiPitch
}

-- There are 12 half tones per octave
semiTonesPerOctave :: Int
semiTonesPerOctave = 12

majorScale :: [Int]
majorScale = [0, 2, 4, 5, 7, 9, 11]

minorNaturalScale :: [Int]
minorNaturalScale = [0, 2, 3, 5, 7, 8, 10]

minorHarmonicScale :: [Int]
minorHarmonicScale = [0, 2, 3, 5, 7, 8, 11]

majorChord :: [Int]
majorChord = [0, 4, 7]

minorChord :: [Int]
minorChord = [0, 3, 7]

-- This is the ascending part of the minor melodic scale.
-- The descending part is the minor natural scale.
minorMelodicScale :: [Int]
minorMelodicScale = [0, 2, 3, 5, 7, 9, 11]

inPattern :: NotesPattern -> MidiPitch -> Bool
inPattern (NotesPattern pattern (MidiPitch root)) (MidiPitch pitch) =
  elem pitchMod pattern
 where
  pitchMod_ = mod ((fromIntegral pitch) - (fromIntegral root)) semiTonesPerOctave
  pitchMod = if pitchMod_ < 0 then pitchMod_ + semiTonesPerOctave else pitchMod_
