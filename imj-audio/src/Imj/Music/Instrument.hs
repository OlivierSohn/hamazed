{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Music.Instrument
      ( -- * Types
        Instrument(..)
      , MusicalEvent(..)
      , InstrumentNote(..)
      , mkInstrumentNote
      , NoteVelocity(..)
      , mkNoteVelocity
      , EnvelopeCharacteristicTime
      , unEnvelopeCharacteristicTime
      -- * Analyze envelope
      , envelopeShape
      -- * Utilities
      , instrumentNoteToMidiPitch
-- * Some instruments
-- | These instruments are used in
-- <https://github.com/OlivierSohn/hamazed/tree/master/imj-game-hamazed Hamazed>.
      , simpleInstrument
      , bellInstrument
      , bell2Instrument
      , organicInstrument
      , shortInstrument
      , testInstrument
      , stringsInstrument
      , longInstrument
      , longBellInstrument
      ) where


import           Imj.Prelude

import           Data.Data(Data(..))
import           GHC.Generics(Generic(..))
import           Control.DeepSeq as Exported(NFData(..))
import           Data.Vector.Unboxed(Vector)

import           Imj.Music.Instruction
import           Imj.Audio.Envelope


data MusicalEvent =
     StartNote !InstrumentNote {-# UNPACK #-} !NoteVelocity
     -- ^ Start playing a note at the given volume
   | StopNote !InstrumentNote
     -- ^ Stop playing a note
  deriving(Generic,Show, Eq)
instance Binary MusicalEvent
instance NFData MusicalEvent


-- | In the range @[0,1]@. 0 means no sound, 1 means full volume.
newtype NoteVelocity = NoteVelocity Float
 deriving (Generic, Num,Show,Eq)
instance Binary NoteVelocity
instance NFData NoteVelocity

-- | Converts a discrete MIDI velocity (0..127) to a continuous velocity (0..1)
mkNoteVelocity :: Int -> NoteVelocity
mkNoteVelocity i
  | i < 1     = NoteVelocity 0
  | i > 126   = NoteVelocity 1
  | otherwise = NoteVelocity $ (fromIntegral i) / 127

-- | Returns lists of consecutive envelope values.
--
-- If the 'Instrument' uses a
-- 'ReleaseMode' 'AutoRelease', a single list is returned, covering all envelope phases, from attack to release.
--
-- If the 'Instrument' uses a
-- 'ReleaseMode' 'KeyRelease', two lists are returned:
--
-- * The first list covers phases from attack to the beginning of sustain.
-- * The second list covers the end of sustain to the release phase.
envelopeShape :: Instrument -> IO [Vector Float]
envelopeShape = \case
  SineSynthAHDSR e ahdsr -> analyzeAHDSREnvelope e ahdsr
  SineSynth _ -> return []
  Wind _ -> return []

-- | A musical instrument (or musical effect).
data Instrument =
    SineSynth !EnvelopeCharacteristicTime
    -- ^ Simple sinus synthethizer, with phase randomization, and trapezoïdal linear envelope.
  | SineSynthAHDSR !ReleaseMode !AHDSR'Envelope
    -- ^ Envelope-based synthethizer, with phase randomization.
  | Wind !Int
  -- ^ Wind sound effect, modelled using filtered noise.
  deriving(Generic,Show, Eq, Data, Ord)
instance Binary Instrument
instance NFData Instrument

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

mkInstrumentNote :: MidiPitch -> Instrument -> InstrumentNote
mkInstrumentNote pitch i =
  InstrumentNote n o i
 where
  (n,o) = midiPitchToNoteAndOctave pitch

{- |
@
   | c |                | c |
       ------------------                  < 1
      .                  .
     .                    .                < s
    .                      .
 ---                        -------------  < 0
   ^                     ^
   |                     |
   key is pressed        key is released
@
-}
newtype EnvelopeCharacteristicTime = EnvelCharacTime {unEnvelopeCharacteristicTime :: Int}
  deriving(Generic, Show, Ord, Eq, Data)
instance Binary EnvelopeCharacteristicTime
instance NFData EnvelopeCharacteristicTime


simpleInstrument, bellInstrument, organicInstrument, shortInstrument, testInstrument, stringsInstrument :: Instrument
longInstrument, longBellInstrument, bell2Instrument :: Instrument
simpleInstrument = SineSynth $ EnvelCharacTime 100
bellInstrument = SineSynthAHDSR AutoRelease $
  AHDSR'Envelope
    500 200 40000 30000
    Linear
    ProportionaValueDerivative
    Linear
    0.01
bell2Instrument = SineSynthAHDSR AutoRelease $
  AHDSR'Envelope
    800 0 50 51200
    (Eased EaseIn Ord5)
    Linear
    (Eased EaseOut Sine)
    1.0
organicInstrument = SineSynthAHDSR AutoRelease
  $ AHDSR'Envelope
      400 5120 50 12800
      (Eased EaseIn Sine)
      Linear
      (Eased EaseOut Circ)
      1.0
shortInstrument = SineSynthAHDSR AutoRelease
  $ AHDSR'Envelope
      800 640 50 3200
      (Eased EaseOut Sine)
      Linear
      (Eased EaseIn Circ)
      1.0
testInstrument = SineSynthAHDSR AutoRelease
  $ AHDSR'Envelope
      800 160 3200 6400
      (Eased EaseInOut Sine)
      ProportionaValueDerivative
      (Eased EaseInOut Circ)
      0.865
stringsInstrument = SineSynthAHDSR KeyRelease
  $ AHDSR'Envelope
      12800 160 3200 6400
      Linear
      Linear
      (Eased EaseInOut Circ)
      1.0
longInstrument = SineSynthAHDSR KeyRelease
  $ AHDSR'Envelope
      50 160 102400 6400
      Linear
      ProportionaValueDerivative
      (Eased EaseInOut Circ)
      0.138
longBellInstrument = SineSynthAHDSR AutoRelease
  $ AHDSR'Envelope
      1600 160 102400 102400
      Linear
      ProportionaValueDerivative
      (Eased EaseOut Sine)
      0.138
