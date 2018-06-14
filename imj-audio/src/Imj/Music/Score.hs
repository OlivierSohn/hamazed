{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Imj.Music.Score
      (
        Score(..)
      , mkScore
      , Voice(..)
      , mkVoice

      ) where

import           Imj.Prelude

import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           Imj.Music.CTypes

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

newtype InstructionIdx = InstructionIdx Int
  deriving(Generic,Show, Num, Integral, Real, Ord, Eq, Enum)
