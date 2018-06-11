{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Imj.Music.Types
      (-- * Modeling keyboard state
        PressedKeys(..)
      , mkEmptyPressedKeys
      -- * Notes and instruments
      -- | In my music notation system, an 'Instrument' partition is modeled as a list of
      -- /monophonic/ music voices with the same time granularity.
      --
      -- A music voice is a list of 'VoiceInstruction's where the nth 'VoiceInstruction'
      -- specifies what the voice should do during the nth time quantum.
      , VoiceInstruction(..)
      , InstrumentNote(..), mkInstrumentNote, instrumentNoteToMidiPitch, noteToMidiPitch
      , Instrument(..)
      , Envelope(..), cycleEnvelope, prettyShowEnvelope
      , EnvelopeCharacteristicTime, mkEnvelopeCharacteristicTime, unEnvelopeCharacteristicTime
      , MidiPitch(..), midiPitchToNoteAndOctave, whiteKeyPitch
      , NoteName(..)
      , whiteKeyNote
      , Octave(..)
      , noOctave
      , NoteVelocity(..)
        -- * Sequences of notes
        -- ** Logical
      , Score(..)
      , mkScore
      , Voice(..)
      , mkVoice
        -- ** Timed
      , AbsolutelyTimedMusicalEvent(..)
      , RelativelyTimedMusicalEvent(..)
      , Recording(..)
      , mkEmptyRecording
      , Sequencer(..)
      , SequencerId(..)
      , MusicLoop(..)
      , mkMusicLoop
        -- * Midi-like instructions
      , MusicalEvent(..)
      -- * Reexport
      , AHDSR(..)
      , Interpolation(..), allInterpolations
      , Ease(..)
      , EasedInterpolation(..)
      , CInt
      ) where

import           Imj.Prelude

import           Control.DeepSeq (NFData(..))
import           Control.Concurrent.MVar.Strict(MVar, newMVar)
import           Data.Binary
import           Data.Data(Data(..))
import           Data.Map.Internal(Map(..))
import qualified Data.Vector as V
import           Foreign.C
import           GHC.Generics (Generic)

import           Imj.Music.CTypes
import           Imj.Timing

-- | Represents the keys currently pressed on a keyboard-based music device.
--
-- The same key can be pressed multiple times for music devices having multiple keyboards
-- with overlapping ranges.
data PressedKeys = PressedKeys !(Map InstrumentNote Int)
  deriving(Generic, Show)
instance Binary PressedKeys
instance NFData PressedKeys

mkEmptyPressedKeys :: PressedKeys
mkEmptyPressedKeys = PressedKeys mempty

-- | A music note played by an 'Instrument'
data InstrumentNote = InstrumentNote !NoteName {-# UNPACK #-} !Octave !Instrument
  deriving(Generic,Show, Eq, Data)
instance Binary InstrumentNote
instance NFData InstrumentNote
instance Ord InstrumentNote where
  compare n@(InstrumentNote _ _ a) m@(InstrumentNote _ _ b) =
    compare (instrumentNoteToMidiPitch n, a) $ (instrumentNoteToMidiPitch m, b)

instrumentNoteToMidiPitch :: InstrumentNote -> MidiPitch
instrumentNoteToMidiPitch (InstrumentNote n oct _) = noteToMidiPitch n oct

-- | According to http://subsynth.sourceforge.net/midinote2freq.html.
--
-- C1 has a 0 pitch
noteToMidiPitch :: NoteName -> Octave -> MidiPitch
noteToMidiPitch n oct = MidiPitch $ 12 * (fromIntegral oct-1) + fromIntegral (fromEnum n)

mkInstrumentNote :: MidiPitch -> Instrument -> InstrumentNote
mkInstrumentNote pitch i =
  InstrumentNote n o i
 where
  (n,o) = midiPitchToNoteAndOctave pitch

midiPitchToNoteAndOctave :: MidiPitch -> (NoteName, Octave)
midiPitchToNoteAndOctave pitch =
  (toEnum n, Octave $ o+1)
 where
  (o,n) = (fromIntegral pitch) `divMod` 12

-- | A 'Score' is a list of 'Voice's
newtype Score = Score [Voice]
  deriving(Generic,Show, Eq)

mkScore :: Instrument -> [[VoiceInstruction]] -> Score
mkScore i s = Score $ map (mkVoice i) s

-- | Contains the instructions to play a voice ('Instrument' and 'VoiceInstruction's)
-- and the state of the voice being played ('InstructionIdx' and current 'VoiceInstruction')
data Voice = Voice {
    _nextIdx :: !InstructionIdx
    -- Index (in 'voiceInstructions') of the 'VoiceInstruction' thaht will be executed
    -- during the next time quantum.
  , _curInstruction :: (Maybe VoiceInstruction)
  -- ^ Can never be 'Just' 'Extend' because when a 'Extend' symbol is encountered, we don't change this value.
  , voiceInstructions :: !(V.Vector VoiceInstruction)
  , voiceInstrument :: !Instrument
} deriving(Generic,Show, Eq)

mkVoice :: Instrument -> [VoiceInstruction] -> Voice
mkVoice i l = Voice 0 Nothing (V.fromList l) i

data VoiceInstruction =
    Note !NoteName {-# UNPACK #-} !Octave
    -- ^ Start playing a music note.
  | Extend
    -- ^ Continue playing the ongoing music note.
  | Rest
    -- ^ Stop playing the ongoing music note, or continue not playing.
  deriving(Generic,Show, Eq, Data)
instance Binary VoiceInstruction
instance NFData VoiceInstruction

data MusicalEvent =
     StartNote !InstrumentNote {-# UNPACK #-} !NoteVelocity
     -- ^ Start playing a note at the given volume
   | StopNote !InstrumentNote
     -- ^ Stop playing a note
  deriving(Generic,Show, Eq)
instance Binary MusicalEvent
instance NFData MusicalEvent

data Envelope =
    KeyRelease
    -- ^ The envelope release is triggered by 'StopNote'
  | AutoRelease
  -- ^ 'StopNote' is not taken into account : the envelope release immediately
  -- follows the envelope decay (the envelope sustain phase is skipped).
  deriving(Generic, Ord, Data, Eq, Show)
instance Enum Envelope where
  fromEnum = \case
    KeyRelease -> 0
    AutoRelease -> 1
  toEnum = \case
    0 -> KeyRelease
    1 -> AutoRelease
    n -> error $ "out of range:" ++ show n
instance NFData Envelope
instance Binary Envelope
prettyShowEnvelope :: Envelope -> String
prettyShowEnvelope = \case
  KeyRelease -> "Key release"
  AutoRelease -> "Autorelease after decay"

cycleEnvelope :: Envelope -> Envelope
cycleEnvelope AutoRelease = KeyRelease
cycleEnvelope e = succ e

-- | A musical instrument (or musical effect).
data Instrument =
    SineSynth !EnvelopeCharacteristicTime
    -- ^ Simple sinus synthethizer, with phase randomization, and trapezoïdal linear envelope.
  | SineSynthAHDSR !Envelope !AHDSR
    -- ^ Simple sinus synthethizer, with phase randomization, and AHDSR envelope.
  | Wind !Int
  -- ^ Wind sound effect, modelled using filtered noise.
  deriving(Generic,Show, Eq, Data, Ord)
instance Binary Instrument
instance NFData Instrument


{- |
@
   | c |                | c |
       __________________                  < 1
      .                  .
     .                    .                < s
    .                      .
 ___                        _____________  < 0
   ^                     ^
   |                     |
   key is pressed        key is released
@
-}
newtype EnvelopeCharacteristicTime = EnvelCharacTime {unEnvelopeCharacteristicTime :: Int}
  deriving(Generic, Show, Ord, Eq, Data)
instance Binary EnvelopeCharacteristicTime
instance NFData EnvelopeCharacteristicTime

mkEnvelopeCharacteristicTime :: Int
                             -- ^ To avoid audio clics, the audio engine will use the max between this value
                             -- and a value representing a "very fast" enveloppe.
                             -> EnvelopeCharacteristicTime
mkEnvelopeCharacteristicTime = EnvelCharacTime . min 100

newtype InstructionIdx = InstructionIdx Int
  deriving(Generic,Show, Num, Integral, Real, Ord, Eq, Enum)

-- | Using the <https://en.wikipedia.org/wiki/Solf%C3%A8ge solgège> notation.
data NoteName =
    Do
  | Réb
  | Ré
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
    Réb -> 1
    Ré -> 2
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
    1 -> Réb
    2 -> Ré
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

-- | Returns 'True' when the 'NoteName' corresponds to a white key on the piano.
whiteKeyNote :: NoteName -> Bool
whiteKeyNote = \case
  Do -> True
  Réb -> False
  Ré -> True
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

noOctave :: Octave
noOctave = Octave 6

-- | In the range @[0,1]@. 0 means no sound, 1 means full volume.
newtype NoteVelocity = NoteVelocity Float
 deriving (Generic, Num,Show,Eq)
instance Binary NoteVelocity
instance NFData NoteVelocity

-- | cf <https://en.wikipedia.org/wiki/MIDI_tuning_standard the midi tuning standard>.
newtype MidiPitch = MidiPitch CShort
  deriving(Show, Ord, Eq, NFData, Generic, Integral, Real, Enum, Num)
instance Binary MidiPitch where
  put (MidiPitch a) = put (fromIntegral a :: Int)
  get = MidiPitch . fromIntegral <$> (get :: Get Int)

-- | A 'MusicalEvent' that happens at an absolute time position.
data AbsolutelyTimedMusicalEvent = ATM !MusicalEvent {-# UNPACK #-} !(Time Point System)
  deriving(Generic,Show)
instance NFData AbsolutelyTimedMusicalEvent

-- | A 'MusicalEvent' that happens at a relative time position.
data RelativelyTimedMusicalEvent = RTM  {
    _rtmMusic :: !MusicalEvent
  , _rtmDt :: {-# UNPACK #-} !(Time Duration System)
} deriving(Generic, Show)
instance NFData RelativelyTimedMusicalEvent

data Recording = Recording {
    _recordedMusic :: ![AbsolutelyTimedMusicalEvent]
    -- ^ Recent events are first in the list.
}  deriving(Generic, Show)
instance NFData Recording

mkEmptyRecording :: Recording
mkEmptyRecording = Recording []

-- | A Sequencer defines a time period (time start + time duration) and has music loops
-- that are played during this period.
data Sequencer k = Sequencer {
    sequenceStart :: !(Time Point System)
    -- ^ The reference time
  , sequencePeriod :: {-# UNPACK #-} !(Time Duration System)
  , musicLoops :: !(Map k MusicLoop)
} deriving(Generic)
instance NFData k => NFData (Sequencer k)

data MusicLoop = MusicLoop {-# UNPACK #-} !(V.Vector RelativelyTimedMusicalEvent) !(MVar PressedKeys)
  deriving(Generic)
instance NFData MusicLoop

mkMusicLoop :: V.Vector RelativelyTimedMusicalEvent -> IO MusicLoop
mkMusicLoop v = MusicLoop v <$> newMVar mkEmptyPressedKeys

newtype SequencerId = SequencerId Int
  deriving(Generic, Show, Ord, Eq, Enum)
instance Binary SequencerId
instance NFData SequencerId
