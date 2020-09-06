{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module Imj.Music.Score
      ( Score(..)
      , mkScore
      , mergeScores
      , scoreLength
      , Voice(..)
      , mkVoice
      , voiceLength
      ) where

import           Imj.Prelude

import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           Imj.Music.Instruction

-- | A 'Score' is a list of 'Voice's
newtype Score i = Score [Voice i]
  deriving(Generic,Show, Eq, Functor)

mkScore :: i -> [[Instruction]] -> Score i
mkScore i s = Score $ map (mkVoice i) s

mergeScores :: Score i -> Score i -> Score i
mergeScores (Score i) (Score j) = Score (i ++ j)

scoreLength :: Score i -> Int
scoreLength (Score l) = fromMaybe 0 $ maximumMaybe $ map voiceLength l


-- | Contains the instructions to play a voice ('Instrument' and 'Instruction's)
-- and the state of the voice being played ('InstructionIdx' and current 'Instruction')
data Voice i = Voice {
    _nextIdx :: !InstructionIdx
    -- Index (in 'voiceInstructions') of the 'Instruction' that will be executed
    -- during the next time quantum.
  , _curInstruction :: (Maybe Instruction)
  -- ^ Can never be 'Just' 'Extend' because when a 'Extend' symbol is encountered, we don't change this value.
  , voiceInstructions :: !(V.Vector Instruction)
  , voiceInstrument :: !i
} deriving(Generic,Show, Eq, Functor)

mkVoice :: i -> [Instruction] -> Voice i
mkVoice i l = Voice 0 Nothing (V.fromList l) i

voiceLength :: Voice i -> Int
voiceLength = V.length . voiceInstructions

newtype InstructionIdx = InstructionIdx Int
  deriving(Generic,Show, Num, Integral, Real, Ord, Eq, Enum)
