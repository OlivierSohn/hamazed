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
      , Oscillator(..)
      , cycleOscillator, countOscillators
      -- * Analyze envelope
      , envelopeShape
      -- * Utilities
      , instrumentNoteToMidiPitch
      , harmonicsFromVolumes
-- * Some instruments
      , marimba
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
      , trapezoidalInstrument
      ) where


import           Imj.Prelude

import           Control.DeepSeq as Exported(NFData(..))
import           Data.Data(Data(..))
import           Data.Vector.Binary()

import           Data.List(dropWhile, foldl')
import           Data.Vector.Unboxed(Vector)
import qualified Data.Vector.Storable as S
import           GHC.Generics(Generic(..))

import           Imj.Audio.Envelope
import           Imj.Audio.Harmonics
import           Imj.Audio.Midi
import           Imj.Music.Instruction

data Oscillator =
    Sinus'VolumeAdjusted
    -- ^ A sinusoïdal oscillator where the volume is adjusted to achieve
    -- <https://en.wikipedia.org/wiki/Equal-loudness_contour equal-loudness>.
  | Sinus
    -- ^ A sinusoïdal oscillator.
  | Saw
    -- ^ A saw oscillator.
  | Square
    -- ^ A square oscillator.
  | Triangle
    -- ^ A triangular oscillator.
  deriving(Generic, Ord, Data, Eq, Show, Bounded)
-- in sync with the corresponding C enum
instance Enum Oscillator where
  fromEnum = \case
    Sinus'VolumeAdjusted -> 0
    Sinus -> 1
    Saw -> 2
    Square -> 3
    Triangle -> 4
  toEnum = \case
    0 -> Sinus'VolumeAdjusted
    1 -> Sinus
    2 -> Saw
    3 -> Square
    4 -> Triangle
    n -> error $ "out of range:" ++ show n
instance NFData Oscillator
instance Binary Oscillator

countOscillators :: Int
countOscillators = 1 + (fromEnum $ (maxBound :: Oscillator))

cycleOscillator :: Int -> Oscillator -> Oscillator
cycleOscillator n v =
  toEnum $ ((fromEnum v) + n) `mod` countOscillators

data MusicalEvent =
     StartNote !(Maybe MidiInfo) !InstrumentNote {-# UNPACK #-} !NoteVelocity
     -- ^ Start playing a note at the given volume
   | StopNote !(Maybe MidiInfo) !InstrumentNote
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
  Synth _ _ e ahdsr -> analyzeAHDSREnvelope e ahdsr
  Wind _ -> return []

-- | A musical instrument (or musical effect).
data Instrument =
    Synth {
        oscillator :: !Oscillator
      , harmonics_ :: !(S.Vector HarmonicProperties)
      , releaseMode_ :: !ReleaseMode
      , envelope_ :: !AHDSR'Envelope
    }
    -- ^ Envelope-based synthethizer, with phase randomization.
  | Wind !Int
  -- ^ Wind sound effect, modelled using filtered noise.
  deriving(Generic,Show, Eq, Data, Ord)
instance Binary Instrument
instance NFData Instrument

defaultHarmonics :: S.Vector HarmonicProperties
defaultHarmonics = harmonicsFromVolumes
 [ 1
 , 0.02
 , 0.005
 , 0.02]

-- | Normalizes the volumes so that the sum of their absolute value is 1
mkHarmonics :: [HarmonicProperties] -> S.Vector HarmonicProperties
mkHarmonics allProps =
  S.fromList $ map (scaleVolume normalizeFactor) props
 where
   props = reverse $ dropWhile ((==) 0 . volume) $ reverse allProps
   sumAbs = foldl' (\s p -> s + abs (volume p)) 0 props
   normalizeFactor = case sumAbs of
     0 -> 1
     _ -> recip sumAbs

-- | Normalizes the volumes so that the sum of their absolute value is 1
harmonicsFromVolumes :: [Float] -> S.Vector HarmonicProperties
harmonicsFromVolumes = mkHarmonics . map (HarmonicProperties 0)

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


marimba :: Instrument
marimba = Synth Sinus'VolumeAdjusted (mkHarmonics $ map (uncurry HarmonicProperties) $ zip [1,0,0,0,0,1] [1.5,0,0,0,0,0.5])
  AutoRelease $
  AHDSR'Envelope
    1600 320 100 25600
    (Eased EaseOut Exp)
    Linear
    (Eased EaseOut Circ)
    1


simpleInstrument, bellInstrument, organicInstrument, shortInstrument, testInstrument, stringsInstrument :: Instrument
longInstrument, longBellInstrument, bell2Instrument :: Instrument
simpleInstrument = Synth Sinus'VolumeAdjusted defaultHarmonics KeyRelease $
  AHDSR'Envelope
    401 0 0 401
    Linear
    Linear
    Linear
    1
bellInstrument = Synth Sinus'VolumeAdjusted defaultHarmonics AutoRelease $
  AHDSR'Envelope
    500 200 40000 30000
    Linear
    ProportionaValueDerivative
    Linear
    0.01
bell2Instrument = Synth Sinus'VolumeAdjusted defaultHarmonics AutoRelease $
  AHDSR'Envelope
    800 0 50 51200
    (Eased EaseIn Ord5)
    Linear
    (Eased EaseOut Sine)
    1.0
organicInstrument = Synth Sinus'VolumeAdjusted defaultHarmonics AutoRelease
  $ AHDSR'Envelope
      400 5120 50 12800
      (Eased EaseIn Sine)
      Linear
      (Eased EaseOut Circ)
      1.0
shortInstrument = Synth Sinus'VolumeAdjusted defaultHarmonics AutoRelease
  $ AHDSR'Envelope
      800 640 50 3200
      (Eased EaseOut Sine)
      Linear
      (Eased EaseIn Circ)
      1.0
testInstrument = Synth Sinus'VolumeAdjusted defaultHarmonics AutoRelease
  $ AHDSR'Envelope
      800 160 3200 6400
      (Eased EaseInOut Sine)
      ProportionaValueDerivative
      (Eased EaseInOut Circ)
      0.865
stringsInstrument = Synth Sinus'VolumeAdjusted defaultHarmonics KeyRelease
  $ AHDSR'Envelope
      12800 160 3200 6400
      Linear
      Linear
      (Eased EaseInOut Circ)
      1.0
longInstrument = Synth Sinus'VolumeAdjusted defaultHarmonics KeyRelease
  $ AHDSR'Envelope
      50 160 102400 6400
      Linear
      ProportionaValueDerivative
      (Eased EaseInOut Circ)
      0.138
longBellInstrument = Synth Sinus'VolumeAdjusted defaultHarmonics AutoRelease
  $ AHDSR'Envelope
      1600 160 102400 102400
      Linear
      ProportionaValueDerivative
      (Eased EaseOut Sine)
      0.138
trapezoidalInstrument :: Int -> Instrument
trapezoidalInstrument i = Synth Sinus'VolumeAdjusted defaultHarmonics KeyRelease
  $ AHDSR'Envelope
      i 0 0 i
      Linear
      ProportionaValueDerivative
      Linear
      1
