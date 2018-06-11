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
--
-- If the 'Instrument' uses an
-- 'Envelope' 'KeyRelease', it will return two lists, the first will be the beginning
-- of the envelope, until the sustain phase begins, the second will start when the key is released.
--
-- If the 'Instrument' uses an
-- 'Envelope' 'AutoRelease', it will return a single list.
envelopeToVectors :: Instrument -> IO [Vector Float]
envelopeToVectors = \case
  SineSynthAHDSR e ahdsr -> analyzeAHDSREnvelope (fromIntegral $ fromEnum e) ahdsr
  SineSynth _ -> return []
  Wind _ -> return []
