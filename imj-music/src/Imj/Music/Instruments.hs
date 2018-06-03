{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Instruments
      ( simpleInstrument
      , bellInstrument
      , organicInstrument
      ) where

import           Imj.Prelude
import           Imj.Music.Types

-- it would be nice to have a "sustain that fades slowly"
-- or maybe what I'm looking for is exponential decay
bell :: AHDSR
bell = AHDSR 500 200 40000 30000 Linear ProportionaValueDerivative Linear 0.01

-- | This instrument is used by default in 'notes' quasi quoter.
simpleInstrument, bellInstrument, organicInstrument :: Instrument
simpleInstrument = SineSynth $ mkEnvelopeCharacteristicTime 401
bellInstrument = SineSynthAHDSR AutoRelease bell
organicInstrument = SineSynthAHDSR AutoRelease
  $ AHDSR
      400 5120 50 12800
      (Eased EaseIn Sine)
      ProportionaValueDerivative
      (Eased EaseOut Circ)
      1.0
