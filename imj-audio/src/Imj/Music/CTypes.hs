{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Music.CTypes
      ( -- | In my music notation system, an 'Instrument' partition is modeled as a list of
        -- /monophonic/ music voices with the same time granularity.
        --
        -- A music voice is a list of 'VoiceInstruction's where the nth 'VoiceInstruction'
        -- specifies what the voice should do during the nth time quantum.
        VoiceInstruction(..)
      , AHDSR(..)
      , Interpolation(..), itpToInt, allInterpolations
      , Ease(..)
      , EasedInterpolation(..)
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
        -- * Midi-like instructions
      , MusicalEvent(..)
      ) where

import           Data.Data(Data(..))
import           GHC.Generics(Generic(..))
import           Control.DeepSeq as Exported(NFData(..))
import           Data.Binary
import           Data.Set(Set)
import qualified Data.Set as Set
import           Foreign.C

{- |
The AHDSR envelope is like an <https://www.wikiaudio.org/adsr-envelope/ ADSR envelope>
, except that the signal can be hold after the attack:

@
   | a |h| d |           |r|
       ---                                      < 1
      .    .
     .       -------------                      < s
    .                     .
 ---                       -------------------  < 0
   ^                     ^
   |                     |
   key is pressed        key is released
@

Attack, decay and release phases can be shaped using an 'Interpolation'.
-}
data AHDSR = AHDSR {
    ahdsrAttack :: {-# UNPACK #-} !Int
    -- ^ Attack duration : the number of samples between when the key is pressed and when the
    -- envelope reaches 1
  , ahdsrHold :: {-# UNPACK #-} !Int
    -- ^ Hold duration: the number of samples during which the full value, 1, will be maintained
    -- after the attack.
  , ahdsrDecay :: {-# UNPACK #-} !Int
    -- ^ Decay duration: the number of samples between the end of the Hold phase and the beginning of
    -- sustain.
  , ahdsrRelease :: {-# UNPACK #-} !Int
    -- ^ Release duration: the number of samples between the moment the key is released and
    -- the moment the envelope reaches 0 (irrespective of wether the envelope had enough time
    -- to reach sustain or not).
  , ahdsrAttackItp :: !Interpolation
  -- ^ Determines how the envelope will be shaped during the attack.
  , ahdsrDecayItp :: !Interpolation
  -- ^ Determines how the envelope will be shaped during the decay.
  , ahdsrReleaseItp :: !Interpolation
  -- ^ Determines how the envelope will be shaped during the release.
  , ahdsrSustain :: {-# UNPACK #-} !Float
  -- ^ The sustain value. Must be in the [0,1] range
} deriving(Generic, Show, Eq, Data, Ord)
instance NFData AHDSR
instance Binary AHDSR

data Ease =
    EaseIn
  | EaseOut
  | EaseInOut
  deriving(Generic, Ord, Eq, Data)
instance Binary Ease
instance NFData Ease

instance Show Ease where
  show = \case
    EaseIn -> "easeIn"
    EaseOut -> "easeOut"
    EaseInOut -> "easeInOut"

allEases :: [Ease]
allEases = [EaseIn, EaseOut, EaseInOut]

-- cf. enum interpolation in interpolation.h
easeBaseIdx :: Ease -> Int
easeBaseIdx = \case
  EaseIn -> 3
  EaseOut -> 12
  EaseInOut -> 21

data Interpolation =
    Linear
  | ProportionaValueDerivative
  | Eased !Ease !EasedInterpolation
  deriving(Generic, Ord, Eq, Data)
instance Binary Interpolation
instance NFData Interpolation

instance Show Interpolation where
  show = \case
    Linear -> "Linear"
    ProportionaValueDerivative -> "PVD"
    Eased e i -> unwords [show e, show i]

allInterpolations :: Set Interpolation
allInterpolations = Set.fromList $
  [Linear, ProportionaValueDerivative] ++
  [Eased e i | e <- allEases, i <- allEasedInterpolations]

-- cf. enum interpolation in interpolation.h
itpToInt :: Interpolation -> Int
itpToInt = \case
  Linear -> 0
  ProportionaValueDerivative -> 1
  Eased e i -> easeBaseIdx e + interpolationIdx i

data EasedInterpolation =
    Ord2
  | Ord3
  | Ord4
  | Ord5
  | Sine
  | Exp
  | Circ
  deriving(Generic, Ord, Eq, Data)
instance Binary EasedInterpolation
instance NFData EasedInterpolation

instance Show EasedInterpolation where
  show = \case
    Ord2 -> "x^2"
    Ord3 -> "x^3"
    Ord4 -> "x^4"
    Ord5 -> "x^5"
    Sine -> "sin"
    Exp -> "exp"
    Circ -> "circ"

allEasedInterpolations :: [EasedInterpolation]
allEasedInterpolations = [Ord2, Ord3, Ord4, Ord5, Sine, Exp, Circ]

-- cf. enum interpolation in interpolation.h
interpolationIdx :: EasedInterpolation -> Int
interpolationIdx = \case
  Ord2 -> 0
  Ord3 -> 1
  Ord4 -> 2
  Ord5 -> 3
  Sine -> 4
  Exp -> 5
  Circ -> 6


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
    -- ^ Envelope-based synthethizer, with phase randomization.
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
