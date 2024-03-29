{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Compositions.Me
      ( me
      , meScore
      , meInstrument
      , meSnare
      , meKickNew
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

meKickNew :: Instrument
meKickNew = Synth
  (Sweep 200 80 EndFreq (Eased EaseIn Exp))
  AutoRelease
  $ AHDSR'Envelope
      50 640 50 12000
      (Eased EaseIn Exp)
      (Linear)
      (Eased EaseOut Exp)
      1.0


meKick :: Instrument
meKick = Synth
  (Sweep 1000 80 EndFreq Linear)
  AutoRelease
  $ AHDSR'Envelope
      100 1200 200 15600
      (Eased EaseIn Exp)
      (Linear)
      (Eased EaseOut Exp)
      0.665


meKick2 :: Instrument
meKick2 = Synth
  (Sweep 50 80 EndFreq Linear)
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

meScore :: Maybe Double
        -- ^  number of seconds per loop
        -> (Double, Score Instrument)
meScore m =
  (fst $ me m,
  mergeScores
    (mkScore meInstrument $ snd $ me m)
    $ mergeScores
      (mkScore meSnare $ allCentered meSnareVoice)
      $ mergeScores
        (mkScore meKick $ allCentered meKickVoice)
        (mkScore meKick2 $ allCentered meKick2Voice)
    )

me :: Maybe Double
   -- ^  number of seconds per loop
   -> (Double,[(NotePan, [Instruction])])
me may_sec_per_loop = (bpm,part)
 where
  bpm = maybe (2*120) (\sec -> num_beats / (sec / 60.0)) may_sec_per_loop

  num_beats = 16*4

  part = allCentered [voices|
    la . la . la . la . la . la . la . la .
    . ^mi . ^re
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
