{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Music.Instruction
      ( -- * Types
        -- | An 'Instrument' partition is modeled as a list of
        -- /monophonic/ music voices with the same time granularity.
        --
        -- A music voice is a list of 'Instruction's where the nth 'Instruction'
        -- specifies what the voice should do during the nth time quantum.
        Instruction(..)
      , NoteName(..)
      , Octave(..)
      , noOctave
      , MidiPitch(..)
      -- * Utilities
      , midiPitchToNoteAndOctave, noteToMidiPitch, noteNameToMidiModuloPitch
      , whiteKeyNote, whiteKeyPitch
      ) where

import           Data.Data(Data(..))
import           GHC.Generics(Generic(..))
import           Control.DeepSeq as Exported(NFData(..))
import           Data.Binary
import           Foreign.C


-- | Using the <https://en.wikipedia.org/wiki/Solf%C3%A8ge solfège> notation.
data NoteName =
    Do
  | Reb
  | Re
  | Mib
  | Mi
  | Fa
  | Solb
  | Sol
  | Lab
  | La
  | Sib
  | Si
  deriving(Generic,Show,Eq, Data)
instance Binary NoteName
instance NFData NoteName
instance Enum NoteName where
  fromEnum = \case
    Do -> 0
    Reb -> 1
    Re -> 2
    Mib -> 3
    Mi -> 4
    Fa -> 5
    Solb -> 6
    Sol -> 7
    Lab -> 8
    La -> 9
    Sib -> 10
    Si -> 11

  toEnum = \case
    0 -> Do
    1 -> Reb
    2 -> Re
    3 -> Mib
    4 -> Mi
    5 -> Fa
    6 -> Solb
    7 -> Sol
    8 -> Lab
    9 -> La
    10 -> Sib
    11 -> Si
    n -> error $ "out of range:" ++ show n

noteNameToMidiModuloPitch :: NoteName -> Int
noteNameToMidiModuloPitch = fromEnum

-- | Returns 'True' when the 'NoteName' corresponds to a white key on the piano.
whiteKeyNote :: NoteName -> Bool
whiteKeyNote = \case
  Do -> True
  Reb -> False
  Re -> True
  Mib -> False
  Mi -> True
  Fa -> True
  Solb -> False
  Sol -> True
  Lab -> False
  La -> True
  Sib -> False
  Si -> True

-- | Returns 'True' when the 'MidiPitch' corresponds to a white key on the piano.
whiteKeyPitch :: MidiPitch -> Bool
whiteKeyPitch = whiteKeyNote . fst . midiPitchToNoteAndOctave

-- | An <https://en.wikipedia.org/wiki/Octave octave>.
newtype Octave = Octave Int
  deriving(Generic,Integral, Real, Num, Enum, Ord, Eq,Show,Binary,NFData, Data)

-- | The base 'Octave' to use for notes.
noOctave :: Octave
noOctave = Octave 6

-- | Represents a <https://en.wikipedia.org/wiki/MIDI_tuning_standard midi pitch>.
newtype MidiPitch = MidiPitch CShort
  deriving(Show, Ord, Eq, NFData, Generic, Integral, Real, Enum, Num)
instance Binary MidiPitch where
  put (MidiPitch a) = put (fromIntegral a :: Int)
  get = MidiPitch . fromIntegral <$> (get :: Get Int)

-- | According to http://subsynth.sourceforge.net/midinote2freq.html, where C1 has a 0 pitch
noteToMidiPitch :: NoteName -> Octave -> MidiPitch
noteToMidiPitch n oct = MidiPitch $ 12 * (fromIntegral oct-1) + fromIntegral (fromEnum n)

midiPitchToNoteAndOctave :: MidiPitch -> (NoteName, Octave)
midiPitchToNoteAndOctave pitch =
  (toEnum n, Octave $ o+1)
 where
  (o,n) = (fromIntegral pitch) `divMod` 12


data Instruction =
    Note !NoteName {-# UNPACK #-} !Octave
    -- ^ Start playing a music note.
  | Extend
    -- ^ Continue playing the ongoing music note.
  | Rest
    -- ^ Stop playing the ongoing music note, or continue not playing.
  deriving(Generic,Show, Eq, Data)
instance Binary Instruction
instance NFData Instruction
