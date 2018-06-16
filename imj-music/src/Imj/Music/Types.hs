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
