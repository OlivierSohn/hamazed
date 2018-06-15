{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}


module Imj.Audio
      (
{- |

The functions exported by this module can be used to play 'Instrument's in real time.

Example:

@
main = usingAudio $ playAtTempo 70 simpleInstrument $ [voice|do ré mi|]
@

-}
        module Imj.Audio.Wrapper
      , module Imj.Music.Alter
      , module Imj.Music.Compose
      , module Imj.Music.Instruments
      , module Imj.Music.Play
      , module Imj.Music.Score
      , module Imj.Music.CTypes
      ) where

import           Imj.Audio.Wrapper
import           Imj.Music.Alter
import           Imj.Music.Compose
import           Imj.Music.Instruments
import           Imj.Music.Play
import           Imj.Music.Score
import           Imj.Music.CTypes
