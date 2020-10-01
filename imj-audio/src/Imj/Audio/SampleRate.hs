{-|
The functions in this module use
<https://github.com/OlivierSohn/cpp.audio/tree/master/include a lockfree, C++17 audio-engine>
to create the audio signal and output it through the
<http://www.portaudio.com/ portaudio> library.

=== Audio thread isolation from GHC runtime

Eventhough during garbage collection, GHC pauses all threads, since
the real-time audio thread is /not/ managed by the GHC runtime, there is
/no/ audio pause during GHC garbage collection.

=== Memory usage

The RAM usage will be proportional to the count of different 'Instrument's
playing music at the same time.

=== Concurrency

All exported functions are thread-safe.

-}

module Imj.Audio.SampleRate
      (
      -- * Globals
      globalSampleRate

      ) where

globalSampleRate :: Int
globalSampleRate = 96000
