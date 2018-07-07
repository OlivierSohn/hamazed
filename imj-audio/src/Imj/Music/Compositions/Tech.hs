{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Compositions.Tech
      ( tech
      , techInstrument
      ) where

import           Imj.Prelude

import           Imj.Audio.Envelope
import           Imj.Music.Compose
import           Imj.Music.Instruction
import           Imj.Music.Instrument

techInstrument :: Instrument
techInstrument = Synth Triangle (harmonicsFromVolumes [1, 0, 0, 0, 0, 1, 0, 0, 0, 1]) AutoRelease
  $ AHDSR'Envelope
      400 5120 50 12800
      (Eased EaseIn Sine)
      Linear
      (Eased EaseOut Circ)
      1.0

tech :: (Float,[[Instruction]])
tech = (bpm,part)
 where
  bpm = 440

  part = [voices|
    la . . mi . . fa . . sol . . . . .

    la . . mi . . fa . . sol . . . . .

    la . . mi . . fa . . sol . . . . .

    la . . mi . . fa . . sol . . . . .

    la . . mi . . fa . . sol . . . . .
    ^^do . . ^sol . . ^la . . ^si . . . . .

    la . . mi . . fa . . sol . . . . .
    ^^do . . ^sol . . ^la . . ^si . . . . .

    la . . mi . . fa . . sol . . . . .
    ^^do . . ^sol . . ^la . . ^si . . . . .

    la . . mi . . fa . . sol . . . . .
    ^^do . . ^sol . . ^la . . ^si . . . . .
  |]
