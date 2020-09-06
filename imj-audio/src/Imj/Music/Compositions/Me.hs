{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Compositions.Me
      ( me
      , meScore
      , meInstrument
      , meSnare
      ) where

import           Imj.Prelude

import           Imj.Audio.Envelope
import           Imj.Music.Compose
import           Imj.Music.Instruction
import           Imj.Music.Instrument
import           Imj.Music.Score



meSnare :: Instrument
meSnare = Synth
  Noise
  AutoRelease
  $ AHDSR'Envelope
      100 1200 200 15600
      (Eased EaseIn Circ)
      (Eased EaseInOut Circ)
      (Eased EaseOut Exp)
      0.665

meInstrument :: Instrument
meInstrument = Synth
  (Oscillations
    Sinus
    (harmonicsFromVolumes [1, 0, 0, 0, 0, 0, 0, 0, 0, 0]))
  AutoRelease
  $ AHDSR'Envelope
      400 5120 50 12800
      (Eased EaseIn Sine)
      Linear
      (Eased EaseOut Circ)
      1.0

meScore :: (Float, Score Instrument)
meScore =
  (fst me,
  mergeScores
    (mkScore meInstrument $ snd me)
    $ mkScore meSnare meSnareVoice)

me :: (Float,[[Instruction]])
me = (bpm,part)
 where
  bpm = 2*120

  part = [voices|
    la . la . la . la . la . la . la . la .
    . ^mi . ^ré
    vvla . . . . vvla vvla vvsi . . . vdo . vvsi vvsol

    fa . fa . fa . fa . fa . fa . fa . fa .
    . ^do . si
    vvfa . . . . vvsol vvsol vvfa . . . . . vvfa vvfa

    sol . sol . sol . sol . sol . sol . sol . sol .
    . si . ^do
    vvsol . . . . vvsol vvla vvsi . . . vvsi . vvla vvsol

    mi . mi . mi . mi . mi . mi . mi . mi .
    . si . ^do
    vvmi . . vvmi . . vvmi . . vvmi . . . vvmi vvmi vvmi
  |]

meSnareVoice :: [[Instruction]]
meSnareVoice = [voices|
    . la
    |]
