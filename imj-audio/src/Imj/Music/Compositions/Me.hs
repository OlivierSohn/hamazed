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
      (Eased EaseIn Exp)
      (Eased EaseInOut Exp)
      (Eased EaseOut Exp)
      0.665

meKick :: Instrument
meKick = Synth
  (Sweep 1000 80 EndFreq)
  AutoRelease
  $ AHDSR'Envelope
      100 1200 200 15600
      (Eased EaseIn Exp)
      (Eased EaseInOut Exp)
      (Eased EaseOut Exp)
      0.665


meKick2 :: Instrument
meKick2 = Synth
  (Sweep 50 80 EndFreq)
  AutoRelease
  $ AHDSR'Envelope
      50 0 50 800
      (Linear)
      (Linear)
      (Eased EaseOut Exp)
      1.0

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
      (Eased EaseOut Exp)
      1.0

meScore :: (Float, Score Instrument)
meScore =
  (fst me,
  mergeScores
    (mkScore meInstrument $ snd me)
    $ mergeScores
      (mkScore meSnare meSnareVoice)
      $ mergeScores
        (mkScore meKick meKickVoice)
        (mkScore meKick2 meKick2Voice)
    )

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

meKickVoice :: [[Instruction]]
meKickVoice = [voices|
    la . la . la . la . la . la . la . . la
    |]
meKick2Voice :: [[Instruction]]
meKick2Voice = [voices|
    . . . . . . . . . . . . . . la .
    |]
