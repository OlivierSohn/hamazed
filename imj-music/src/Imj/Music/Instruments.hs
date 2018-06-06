{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Instruments
      ( simpleInstrument
      , bellInstrument
      , bell2Instrument
      , organicInstrument
      , shortInstrument
      , testInstrument
      , stringsInstrument
      , longInstrument
      , longBellInstrument
      ) where

import           Imj.Prelude
import           Imj.Music.Types


-- | This instrument is used by default in 'notes' quasi quoter.
simpleInstrument, bellInstrument, organicInstrument, shortInstrument, testInstrument, stringsInstrument :: Instrument
longInstrument, longBellInstrument, bell2Instrument :: Instrument
simpleInstrument = SineSynth $ mkEnvelopeCharacteristicTime 401
bellInstrument = SineSynthAHDSR AutoRelease $
  AHDSR
    500 200 40000 30000
    Linear
    ProportionaValueDerivative
    Linear
    0.01
bell2Instrument = SineSynthAHDSR AutoRelease $
  AHDSR
    800 0 50 51200
    (Eased EaseIn Ord5)
    Linear
    (Eased EaseOut Sine)
    1.0
organicInstrument = SineSynthAHDSR AutoRelease
  $ AHDSR
      400 5120 50 12800
      (Eased EaseIn Sine)
      Linear
      (Eased EaseOut Circ)
      1.0
shortInstrument = SineSynthAHDSR AutoRelease
  $ AHDSR
      800 640 50 3200
      (Eased EaseOut Sine)
      Linear
      (Eased EaseIn Circ)
      1.0
testInstrument = SineSynthAHDSR AutoRelease
  $ AHDSR
      800 160 3200 6400
      (Eased EaseInOut Sine)
      ProportionaValueDerivative
      (Eased EaseInOut Circ)
      0.865
stringsInstrument = SineSynthAHDSR KeyRelease
  $ AHDSR
      12800 160 3200 6400
      Linear
      Linear
      (Eased EaseInOut Circ)
      1.0
longInstrument = SineSynthAHDSR KeyRelease
  $ AHDSR
      50 160 102400 6400
      Linear
      ProportionaValueDerivative
      (Eased EaseInOut Circ)
      0.138
longBellInstrument = SineSynthAHDSR AutoRelease
  $ AHDSR
      1600 160 102400 102400
      Linear
      ProportionaValueDerivative
      (Eased EaseOut Sine)
      0.138
