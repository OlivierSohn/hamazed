{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Music.Instrument
      ( -- * Types
        Instrument(..)
      , Harmonics(..)
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
      , kick
-- | These instruments are used in
-- <https://github.com/OlivierSohn/hamazed/tree/master/imj-game-hamazed Hamazed>.
      , simpleInstrument
      , bellInstrument
      , bell2Instrument
      , organicInstrument
      , shortInstrument
      , testInstrument
      , stringsInstrument
      , synthInstrument
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
import           Imj.Data.AlmostFloat
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

data MusicalEvent instr =
     StartNote !(Maybe MidiInfo) !(InstrumentNote instr) {-# UNPACK #-} !NoteVelocity
     -- ^ Start playing a note at the given volume
   | StopNote !(Maybe MidiInfo) !(InstrumentNote instr)
     -- ^ Stop playing a note
  deriving(Generic,Show, Eq)
instance Binary instr => Binary (MusicalEvent instr)
instance NFData instr => NFData (MusicalEvent instr)
instance Functor MusicalEvent where
  {-# INLINABLE fmap #-}
  fmap f = \case
    StartNote a b c -> StartNote a (fmap f b) c
    StopNote a b -> StopNote a $ fmap f b

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
envelopeShape :: Instrument -> IO [Vector Double]
envelopeShape = \case
  Synth _ _ e ahdsr -> analyzeAHDSREnvelope e ahdsr
  Wind _ -> return []

-- | A musical instrument (or musical effect).
data Instrument =
    Synth {
        oscillator   :: !Oscillator
      , harmonics_   :: !Harmonics
      , releaseMode_ :: !ReleaseMode
      , envelope_    :: !AHDSR'Envelope
    }
    -- ^ Envelope-based synthethizer, with phase randomization.
  | Wind !Int
  -- ^ Wind sound effect, modelled using filtered noise.
  deriving(Generic,Show, Eq, Data, Ord)
instance Binary Instrument
instance NFData Instrument

defaultHarmonics :: Harmonics
defaultHarmonics = harmonicsFromVolumes
 [ 1
 , 0.02
 , 0.005
 , 0.02]


newtype Harmonics = Harmonics { unHarmonics :: S.Vector HarmonicProperties }
  deriving(Generic,Show, Data)
instance Binary Harmonics
instance NFData Harmonics
instance Eq Harmonics where
  {-# INLINABLE (==) #-}
  (Harmonics v1) == (Harmonics v2) =
    let l1 = lastRelevantHarmonicIndex v1
        l2 = lastRelevantHarmonicIndex v2
    in if l1 == l2
          then
            maybe
              True
              (\lastIdx ->
                let len = lastIdx + 1
                -- verify that the l1 + 1 first harmonics are equal.
                in S.unsafeTake len v1 == S.unsafeTake len v2
              )
              l1
          else
            False
instance Ord Harmonics where
  {-# INLINABLE compare #-}
  (Harmonics v1) `compare` (Harmonics v2) = case compare l1 l2 of
    EQ ->
      maybe
        EQ
        (\lastIdx ->
          let len = lastIdx + 1
          in (S.unsafeTake len v1) `compare` (S.unsafeTake len v2))
        l1
    other -> other
   where
    l1 = lastRelevantHarmonicIndex v1
    l2 = lastRelevantHarmonicIndex v2

lastRelevantHarmonicIndex :: S.Vector HarmonicProperties -> Maybe Int
lastRelevantHarmonicIndex =
  fst . S.foldl'
    (\(lastNonZero,idx) hp ->
      let newLNZ =
            if volume hp == 0
              then
                lastNonZero
              else
                Just idx
      in (newLNZ, idx+1))
    (Nothing,0)

-- | Normalizes the volumes so that the sum of their absolute value is 1
mkHarmonics :: [HarmonicProperties] -> Harmonics
mkHarmonics allProps =
  Harmonics $ S.fromList $ map (scaleVolume normalizeFactor) props
 where
   props = reverse $ dropWhile ((==) 0 . volume) $ reverse allProps
   sumAbs = foldl' (\s p -> s + abs (volume p)) 0 props
   normalizeFactor = case sumAbs of
     0 -> 1
     _ -> recip sumAbs

-- | Normalizes the volumes so that the sum of their absolute value is 1
harmonicsFromVolumes :: [Float] -> Harmonics
harmonicsFromVolumes = mkHarmonics . map (HarmonicProperties 0 . almost)

-- | A music note played by an 'Instrument'
data InstrumentNote instr = InstrumentNote !NoteName {-# UNPACK #-} !Octave !instr
  deriving(Generic,Show, Eq)
instance Binary instr => Binary (InstrumentNote instr)
instance NFData instr => NFData (InstrumentNote instr)
instance Ord i => Ord (InstrumentNote i) where
  compare n@(InstrumentNote _ _ a) m@(InstrumentNote _ _ b) =
    compare (instrumentNoteToMidiPitch n, a) $ (instrumentNoteToMidiPitch m, b)
instance Functor InstrumentNote where
  {-# INLINABLE fmap #-}
  fmap f (InstrumentNote n o i) = InstrumentNote n o $ f i

instrumentNoteToMidiPitch :: InstrumentNote i -> MidiPitch
instrumentNoteToMidiPitch (InstrumentNote n oct _) = noteToMidiPitch n oct

mkInstrumentNote :: MidiPitch -> i -> InstrumentNote i
mkInstrumentNote pitch i =
  InstrumentNote n o i
 where
  (n,o) = midiPitchToNoteAndOctave pitch


kick :: Instrument
kick = Synth Sinus'VolumeAdjusted (mkHarmonics $ map (uncurry HarmonicProperties) $ zip [1,1,1,1,1,1,1,1,1,1] [1.4,0.2,0.4,0.9,1.9,0.5,1.9,0.3,0.2,0])
  AutoRelease $
  AHDSR'Envelope
    50 0 50 50
    (Eased EaseInOut Exp)
    (Eased EaseInOut Exp)
    Linear
    0

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
longInstrument, longBellInstrument, bell2Instrument, synthInstrument :: Instrument
simpleInstrument = Synth Sinus'VolumeAdjusted defaultHarmonics KeyRelease $
  AHDSR'Envelope
    401 0 0 401
    Linear
    Linear
    Linear
    1
synthInstrument = Synth
  Triangle {-Square makes a nice variation -}
  (harmonicsFromVolumes [1,1,0,1,0,0,0,0.1]) AutoRelease $
  AHDSR'Envelope
    100 2560 100 12800
    (Eased EaseInOut Circ)
    Linear
    (Eased EaseOut Circ)
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
