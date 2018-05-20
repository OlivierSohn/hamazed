{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Imj.Music.Types
      ( -- * Notes and instruments
        Symbol(..)
      , NoteSpec(..)
      , Instrument(..)
      , MidiPitch(..)
      , NoteName(..)
      , naturalNote
      , Octave(..)
      , noOctave
      , MidiVelocity(..)
        -- * Sequences of notes
        -- ** Logical
      , Score(..)
      , mkScore
      , Voice(..)
      , mkVoice
        -- ** Timed
      , AbsoluteTimedMusic(..)
      , RelativeTimedMusic(..)
      , Recording(..)
      , mkEmptyRecording
      , Sequencer(..)
      , SequencerId(..)
      , MusicLine(..)
      , mkMusicLine
      , PianoState(..)
      , mkEmptyPiano
        -- * Midi-like instructions
      , Music(..)
      -- * Reexport
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

import           Imj.Timing

-- | Represents the keys currently pressed. Note that the same key can be pressed
-- twice if the lower and upper keyboards overlapp.
data PianoState = PianoState !(Map NoteSpec Int)
  deriving(Generic, Show)
instance Binary PianoState
instance NFData PianoState

mkEmptyPiano :: PianoState
mkEmptyPiano = PianoState mempty

data Symbol =
    Note {-# UNPACK #-} !NoteSpec
  | Extend
  | Rest
  deriving(Generic,Show, Eq, Data)
instance Binary Symbol
instance NFData Symbol

data NoteSpec = NoteSpec !NoteName {-# UNPACK #-} !Octave
  deriving(Generic,Show, Eq, Data)
instance Enum NoteSpec where
  fromEnum (NoteSpec n oct) = 12 * (fromIntegral oct-1) + fromEnum n
  toEnum pitch =
    NoteSpec (toEnum n) $ Octave $ o+1
   where
    (o,n) = pitch `divMod` 12
instance Ord NoteSpec where
  compare n m = compare (fromEnum n) $ fromEnum m
instance Binary NoteSpec
instance NFData NoteSpec

data Score = Score {
  _voices :: ![Voice]
} deriving(Generic,Show, Eq)

mkScore :: [[Symbol]] -> Score
mkScore = Score . map mkVoice

-- | Keeps track of progress, and loops when the end is found.
data Voice = Voice {
    _nextIdx :: !NoteIdx
  , _curNote :: (Maybe Symbol)
  -- ^ The invariant is that this can never be 'Just' 'Extend' : instead,
  -- when a 'Extend' symbol is encountered, we don't change this value.
  , voiceSymbols :: !(V.Vector Symbol)
} deriving(Generic,Show, Eq)

mkVoice :: [Symbol] -> Voice
mkVoice l = Voice 0 Nothing $ V.fromList l

-- | A music fragment
data Music =
     StartNote !NoteSpec {-# UNPACK #-} !MidiVelocity
   | StopNote !NoteSpec
  deriving(Generic,Show, Eq)
instance Binary Music
instance NFData Music

data Instrument =
    SineSynth
  | Wind !Int
  deriving(Generic,Show, Eq)
instance Binary Instrument
instance NFData Instrument

newtype NoteIdx = NoteIdx Int
  deriving(Generic,Show, Num, Integral, Real, Ord, Eq, Enum)

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

naturalNote :: NoteName -> Bool
naturalNote = \case
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

newtype Octave = Octave Int
  deriving(Generic,Integral, Real, Num, Enum, Ord, Eq,Show,Binary,NFData, Data)
noOctave :: Octave
noOctave = Octave 6

newtype MidiVelocity = MidiVelocity Float
 deriving (Generic, Num,Show,Eq)
instance Binary MidiVelocity
instance NFData MidiVelocity

newtype MidiPitch = MidiPitch CShort
  deriving(Show, Ord, Eq, NFData, Generic, Integral, Real, Enum, Num)
instance Binary MidiPitch where
  put (MidiPitch a) = put (fromIntegral a :: Int)
  get = MidiPitch . fromIntegral <$> (get :: Get Int)

data AbsoluteTimedMusic = ATM !Music !Instrument {-# UNPACK #-} !(Time Point System)
  deriving(Generic,Show)
instance NFData AbsoluteTimedMusic

data RelativeTimedMusic = RTM  {
    _rtmMusic :: !Music
  , _rtmInstrument :: !Instrument
  , _rtmDt :: {-# UNPACK #-} !(Time Duration System)
} deriving(Generic, Show)
instance NFData RelativeTimedMusic

data Recording = Recording {
    _recordedMusic :: ![AbsoluteTimedMusic]
    -- ^ Ordered by inverse time.
}  deriving(Generic, Show)
instance NFData Recording

mkEmptyRecording :: Recording
mkEmptyRecording = Recording []

data Sequencer k = Sequencer {
    sequenceStart :: !(Time Point System)
  , sequencePeriod :: {-# UNPACK #-} !(Time Duration System)
  , musicLines :: !(Map k MusicLine)
} deriving(Generic)
instance NFData k => NFData (Sequencer k)

data MusicLine = MusicLine {-# UNPACK #-} !(V.Vector RelativeTimedMusic) !(MVar PianoState)
  deriving(Generic)
instance NFData MusicLine

mkMusicLine :: (V.Vector RelativeTimedMusic) -> IO MusicLine
mkMusicLine v = MusicLine v <$> newMVar mkEmptyPiano

newtype SequencerId = SequencerId Int
  deriving(Generic, Show, Ord, Eq, Enum)
instance Binary SequencerId
instance NFData SequencerId
