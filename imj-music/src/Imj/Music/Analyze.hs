{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Music.Analyze
      ( envelopeToVectors
      ) where

import           Imj.Prelude
import           Data.Vector.Unboxed(Vector)

import           Imj.Audio
import           Imj.Music.Types

-- | Returns a list of consecutive envelope values.
envelopeToVectors :: Instrument -> IO [Vector Float]
envelopeToVectors = \case
  SineSynthAHDSR e ahdsr -> analyzeAHDSREnvelope (fromIntegral $ fromEnum e) ahdsr
  SineSynth _ -> return []
  Wind _ -> return []
