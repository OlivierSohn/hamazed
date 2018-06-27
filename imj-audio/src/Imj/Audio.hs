{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}


 module Imj.Audio
      (
      {- |
      The functions exported by this module can be used to play 'Instrument's in real time:

@
main = usingAudioOutput $ playAtTempo 70 simpleInstrument $ [voice|do ré mi|]
@
      -}
      module Imj.Audio.Output
      , module Imj.Music.Play
      , module Imj.Music.Instrument
      , module Imj.Audio.Envelope
-- * Music writing

      {-|
      Music-writing is fast and intuitive:

      * the melody is written using note names
      * @^@ and @v@ indicate octave shifts
      * @.@ indicates a pause
      * @-@ indicates that the previous note is extended

      For example, the begining of <https://www.youtube.com/watch?v=GRxofEmo3HA Four seasons, by Vivaldi>
      could be written /monophonically/ using 'voice', in C Major (Do Majeur):

@
[voice|
  do .
  mi . mi . mi . ré do sol - - - - . sol fa
  mi . mi . mi . ré do sol - - - - . sol fa
  mi . fa sol fa . mi . ré . vsi . vsol .
|]

To match the original tonality, which is E Major (Mi Majeur)
we can use 'map (transposeSymbol 4)' on the result,
where 4 is the number of semitones between C (Sol) and E (Mi).
@

      The same piece could be written /polyphonically/ using 'voices',
      where a blank line separates two polyphonic systems:

@
[voices|
  do -
  vsol -

  mi . mi . mi  .  ré  do  sol - -   - -  . sol fa
  do . do . vsi do vsi vla vsi . .   . ré . mi  ré

  mi . mi . mi  .  ré  do  sol ré   sol -  -  mi sol fa
  do . do . vsi do vsi vla vsi vsol vsi do ré .  mi  ré

  mi . fa sol fa .   mi .   ré   .    vsi  .    vsol .
  do . ré mi  ré vsi do vla vsol vfa\# vsol vfa\# vsol .

  do   -
  vsol -

  mi . mi . mi  .  ré  do  sol -   -  -   -  .   sol fa
  do . do . vsi do vsi vla vsi -   do do  ré .   mi  ré
  .  . .  . .   .  .   .   .   sol .  sol .  ^do .

  mi sol mi . mi  .  ré  do  sol la   sol -  -  la sol fa
  do .   do . vsi do vsi vla vsi vsol vsi do ré .  mi  ré

  mi . fa sol fa .   mi .   ré   .    vsi  .    vsol .
  do . ré mi  ré vsi do vla vsol vfa\# vsol vfa\# vsol .
|]
@
      -}
      , module Imj.Music.Compose
      , module Imj.Music.Alter
      , module Imj.Music.Score
      , module Imj.Music.Instruction
      ) where

import           Imj.Audio.Output
import           Imj.Audio.Envelope
import           Imj.Music.Alter
import           Imj.Music.Compose
import           Imj.Music.Instrument
import           Imj.Music.Play
import           Imj.Music.Score
import           Imj.Music.Instruction
