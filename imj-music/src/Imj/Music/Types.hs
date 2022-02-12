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
      -- A music voice is a list of 'Instruction's where the nth 'Instruction'
      -- specifies what the voice should do during the nth time quantum.
      , Instruction(..)
        -- * Sequences of notes
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
      ) where

import           Imj.Prelude

import           Control.Concurrent.MVar.Strict(MVar, newMVar)
import           Data.Map.Internal(Map(..))
import qualified Data.Vector as V

import           Imj.Music.Instruction
import           Imj.Music.Instrument
import           Imj.Timing

-- | Represents the keys currently pressed on a keyboard-based music device.
--
-- The same key can be pressed multiple times for music devices having multiple keyboards
-- with overlapping ranges.
data PressedKeys i = PressedKeys !(Map (InstrumentNote i) Int)
  deriving(Generic, Show)
instance (Binary i) => Binary (PressedKeys i)
instance (NFData i) => NFData (PressedKeys i)

mkEmptyPressedKeys :: Ord i => PressedKeys i
mkEmptyPressedKeys = PressedKeys mempty


-- | A 'MusicalEvent' that happens at an absolute time position.
data AbsolutelyTimedMusicalEvent i = ATM !(MusicalEvent i) {-# UNPACK #-} !(Time Point System)
  deriving(Generic,Show)
instance (NFData i) => NFData (AbsolutelyTimedMusicalEvent i)

-- | A 'MusicalEvent' that happens at a relative time position.
data RelativelyTimedMusicalEvent i = RTM  {
    _rtmMusic :: !(MusicalEvent i)
  , _rtmDt :: {-# UNPACK #-} !(Time Duration System)
} deriving(Generic, Show)
instance NFData i => NFData (RelativelyTimedMusicalEvent i)

data Recording i = Recording {
    _recordedMusic :: ![AbsolutelyTimedMusicalEvent i]
    -- ^ Recent events are first in the list.
}  deriving(Generic, Show)
instance NFData i => NFData (Recording i)

mkEmptyRecording :: Recording i
mkEmptyRecording = Recording []

-- | A Sequencer defines a time period (time start + time duration) and has music loops
-- that are played during this period.
data Sequencer k i = Sequencer {
    sequenceStart :: !(Time Point System)
    -- ^ The reference time
  , sequencePeriod :: {-# UNPACK #-} !(Time Duration System)
  , musicLoops :: !(Map k (MusicLoop i))
} deriving(Generic)
instance (NFData k, NFData i) => NFData (Sequencer k i)

data MusicLoop i = MusicLoop {-# UNPACK #-} !(V.Vector (RelativelyTimedMusicalEvent i)) !(MVar (PressedKeys i))
  deriving(Generic)
instance NFData i => NFData (MusicLoop i)

mkMusicLoop :: (NFData i, Ord i) => V.Vector (RelativelyTimedMusicalEvent i) -> IO (MusicLoop i)
mkMusicLoop v = MusicLoop v <$> newMVar mkEmptyPressedKeys

newtype SequencerId = SequencerId Int
  deriving(Generic, Show, Ord, Eq, Enum)
instance Binary SequencerId
instance NFData SequencerId
