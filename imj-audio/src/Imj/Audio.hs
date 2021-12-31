{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}


 module Imj.Audio
      (
      {- |
      The functions exported by this module can be used to play 'Instrument's in real time:

@
main = usingAudioOutput $ playAtTempo 70 simpleInstrument $ [voice|do re mi|]
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
  mi . mi . mi . re do sol - - - - . sol fa
  mi . mi . mi . re do sol - - - - . sol fa
  mi . fa sol fa . mi . re . vsi . vsol .
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

  mi . mi . mi  .  re  do  sol - -   - -  . sol fa
  do . do . vsi do vsi vla vsi . .   . re . mi  re

  mi . mi . mi  .  re  do  sol re   sol -  -  mi sol fa
  do . do . vsi do vsi vla vsi vsol vsi do re .  mi  re

  mi . fa sol fa .   mi .   re   .    vsi  .    vsol .
  do . re mi  re vsi do vla vsol vfa\# vsol vfa\# vsol .

  do   -
  vsol -

  mi . mi . mi  .  re  do  sol -   -  -   -  .   sol fa
  do . do . vsi do vsi vla vsi -   do do  re .   mi  re
  .  . .  . .   .  .   .   .   sol .  sol .  ^do .

  mi sol mi . mi  .  re  do  sol la   sol -  -  la sol fa
  do .   do . vsi do vsi vla vsi vsol vsi do re .  mi  re

  mi . fa sol fa .   mi .   re   .    vsi  .    vsol .
  do . re mi  re vsi do vla vsol vfa\# vsol vfa\# vsol .
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
