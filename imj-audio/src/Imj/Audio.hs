{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

{- |

== Overview

The functions exported by this module can be used to play 'Instrument's in real time.

Example:

@
main = usingAudio $ playAtTempo 70 simpleInstrument $ [voice|do ré mi|]
@

== C++ Audio engine

Behind the scenes,
<https://github.com/OlivierSohn/cpp.audio/tree/master/include this C++ audio engine>
is used, through
<https://github.com/OlivierSohn/hamazed/tree/master/imj-audio these bindings>.

The <http://www.portaudio.com/ portaudio> c library is used to talk to your audio system.

=== Memory usage

The RAM usage is proportional to the number of /active/ instruments at any given time.

To prevent memory fragmentation, we /recycle/ C++ objects associated to instruments.

=== Audio thread isolation

The real-time audio thread is /not/ managed by the GHC runtime, so there is
/no/ audio pause during GHC garbage collection.

=== Concurrency

Deadlocks are avoided in the audio engine by acquiring locks according to the same global order,
everywhere in the code.

=== Tackling priority inversion

The audio engine uses the
<https://en.wikipedia.org/wiki/Priority_ceiling_protocol Immediate Ceiling Priority Protocol>
to avoid
<https://en.wikipedia.org/wiki/Priority_inversion priority inversion>.

On linux, controling the thread priorities can be done only if the user running the program
<http://pubs.opengroup.org/onlinepubs/009696899/functions/pthread_getschedparam.html has sufficient privileges>
, hence it is preferable if you can run the program with @sudo@.
If not, a warning message will be logged in the console, and the program will run,
but with less guarantees about audio "smoothness" because we can potentially see
<https://en.wikipedia.org/wiki/Priority_inversion priority inversion effects>.

-}

module Imj.Audio
      ( module Imj.Audio.Wrapper
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
