{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Compositions.Tech
      ( tech
      , tech2
      , techInstrument
      ) where

import           Imj.Prelude

import           Imj.Audio.Envelope
import           Imj.Music.Compose
import           Imj.Music.Instruction
import           Imj.Music.Instrument

techInstrument :: Instrument
techInstrument = Synth
  (Oscillations
    Triangle
    (harmonicsFromVolumes [1, 0, 0, 0, 0, 1, 0, 0, 0, 1]))
  AutoRelease
  $ AHDSR'Envelope
      400 5120 50 12800
      (Eased EaseIn Sine)
      Linear
      (Eased EaseOut Exp)
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


tech2 :: (Float,[[Instruction]])
tech2 = (bpm,part)
 where
  bpm = 440

  part = [voices|
    sol .
    mib .
    do .

    do . do . do . do . do .

    sol . fa  .
    mib . mib .
    do  . do  .

    do . do . do . do . do . do .

    fa  . fa  .
    mib . mib .
    do  . do  .

    do . do . do . do . do . do .

    fa  .
    mib . mib .
    do  . do  .

    do . do . do . do . do . do .

    mib . mib .
    do  . do  .


    do . do . do . do . do . do .

    mib . ré .
    do  . do .

    do .

    ré .
    do .

    do .

    mib .
    do .

    do .

    fa .
    do .

    do . do .

    sib .
    sol .
    mib .
    do .

    do . do . do . do . do .

    sib . la  .
    sol . fa  .
    mib . mib .
    do  . do  .

    do . do . do . do . do . do .

    la  . la  .
    fa  . fa  .
    mib . mib .
    do  . do  .

    do . do . do . do . do . do .

    la  . sol .
    fa  .
    mib . mib .
    do  . do  .

    do . do . do . do . do . do .

    sol . sol .
    mib . mib .
    do  . do  .

    do . do . do . do . do . do .

    sol . fa .
    mib . ré .
    do  . do .

    do .

    fa .
    ré .
    do .

    do .

    sol .
    mib .
    do .

    do .

    sol .
    fa .
    do .

    do . do .
  |]
