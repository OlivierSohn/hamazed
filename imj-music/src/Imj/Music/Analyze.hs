{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Music.Analyze
      ( envelopeShape
      ) where

import           Imj.Prelude
import           Data.Vector.Unboxed(Vector)

import           Imj.Audio
import           Imj.Music.Types

-- | Returns lists of consecutive envelope values.
--
-- If the 'Instrument' uses an
-- 'Envelope' 'AutoRelease', a single list is returned, covering all envelope phases, from attack to release.
--
-- If the 'Instrument' uses an
-- 'Envelope' 'KeyRelease', two lists are returned:
--
-- * The first list covers phases from attack to the beginning of sustain.
-- * The second list covers the end of sustain to the release phase.
envelopeShape :: Instrument -> IO [Vector Float]
envelopeShape = \case
  SineSynthAHDSR e ahdsr -> analyzeAHDSREnvelope (fromIntegral $ fromEnum e) ahdsr
  SineSynth _ -> return []
  Wind _ -> return []
