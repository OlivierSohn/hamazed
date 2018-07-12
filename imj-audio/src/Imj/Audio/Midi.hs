{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Audio.Midi
  ( MidiInfo(..)
  , MidiSourceIdx(..)
  , mkMidiSourceIdx
  , MaxMIDIJitter(..)
  , defaultMaxMIDIJitter
  ) where

import Imj.Prelude
import           Data.Word(Word64, Word)

-- | Buffer size, in microseconds, introduced to avoid MIDI jitter
newtype MaxMIDIJitter = MaxMIDIJitter { unMaxMIDIJitter :: Word64 }
  deriving(Num, Integral, Real, Enum, Show, Eq, Ord)
defaultMaxMIDIJitter :: MaxMIDIJitter
defaultMaxMIDIJitter = 0


-- | Must be between 0 and 16383, because the audioengine can encode
-- only that many different MIDI sources.
newtype MidiSourceIdx = MidiSourceIdx {unMidiSourceIdx :: Word}
  deriving (Generic, Show, Eq)
instance NFData MidiSourceIdx
instance Binary MidiSourceIdx

mkMidiSourceIdx :: Int
                -- ^ Will be moduloed to be in 0..16383 range
                -> MidiSourceIdx
mkMidiSourceIdx i = MidiSourceIdx $ fromIntegral $ i `mod` 16384

data MidiInfo = MidiInfo {
    timestamp :: {-# UNPACK #-} !Word64
  , source :: {-# UNPACK #-} !MidiSourceIdx
} deriving (Generic, Show, Eq)
instance NFData MidiInfo
instance Binary MidiInfo
