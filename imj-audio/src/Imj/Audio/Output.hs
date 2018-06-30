{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Imj.Audio.Output
      ( -- * Bracketed init / teardown
        usingAudioOutput
      , usingAudioOutputWithMinLatency
      -- * Playing music
      , play
      , MusicalEvent(..)
      -- * C++ audio-engine implementation details
      {-|

      === Design evolution, from lockfull to lockfree

      To synchronize accesses to shared data between
      the audio realtime thread and non realtime threads, a locking approach was first used.

      ==== Lockfull

      This approach, while being easy to implement and reason about, lead to
      <https://en.wikipedia.org/wiki/Priority_inversion priority inversion> issues:
      sometimes, the realtime trhead would have to wait (a too-long time)
      for a non realtime thread to release the lock. This caused
      audio output buffer overflows, and audio glitches.

      ==== Lockfull + thread priority adjustment

      To fix priority inversion, we raised the priority of every thread
      that was aiting to acquire the lock, and lowered the priority of the thread
      once the lock was released. Eventhough this approach was theoretically sound,
      it has proven ineffective in practice (maybe because the priority change doesn't
      take effect immediately?) and was requiring the program to be run under sudo
      on Linux, to grant the rights to modify thread priorities.

      ==== Lockfree

      A second, successfull attempt was made to fix the priority inversion by
      removing the need to lock in the realtime thread:
      using lockfree datastructures and algorithms, we could synchronize access
      to shared data without ever locking (and without ever waiting in the realtime thread).

      Both modes (lockfull, lockfree) are available today via C++ template parametrization.
      The lockfree mode is the default, and recommended mode today, as it allows to smoothly
      output audio, even under contention.

      When compiling the package, you can revert to the old and unsafe lockfull mode
      by activating the 'Lock' flag.

      === Benchmarks

      When compiling the package with the 'LogTime' flag, the program will write in the console,
      every 1000 audio callback calls, the max and average durations of the audio callback.

      -}

      ) where

import           Control.Monad.IO.Unlift(MonadUnliftIO, liftIO)
import           Data.Bool(bool)
import           Data.Text(Text)
import           Foreign.C
import           UnliftIO.Exception(bracket)

import           Imj.Audio.Envelope
import           Imj.Music.Instruction
import           Imj.Music.Instrument
import           Imj.Timing

-- |
-- * initializes the global audio output stream if it is not initialized yet,
-- * then runs the action containing calls to 'play', and possibly reentrant calls
--     to 'usingAudioOutput' or 'usingAudioOutputWithMinLatency'
-- * then if this is currently the only call to 'usingAudioOutput' or 'usingAudioOutputWithMinLatency'
--     in the program, the audio output signal is swiftly cross-faded to zero
--     and the global audio output stream is uninitialized.
--
-- This function is thread-safe because the initialization and teardown of the
-- global audio output stream are protected by a lock.
--
-- This function can recursively call 'usingAudioOutput' or
-- 'usingAudioOutputWithMinLatency' in the action passed as parameter.
usingAudioOutput :: MonadUnliftIO m
                 => m a
                 -> m (Either Text a)
usingAudioOutput = usingAudioOutputWithMinLatency $ fromSecs 0.008

-- | Same as 'usingAudioOutput' except that the minimum latency can be configured.
--
-- Note that the latency parameter will be used only if there is currently no other active call to
-- 'usingAudioOutput' or 'usingAudioOutputWithMinLatency', else it is ignored (in that case,
-- the global audio output stream is already initialized).
usingAudioOutputWithMinLatency :: MonadUnliftIO m
                               => Time Duration System
                               -- ^ The minimum latency of the audio output stream.
                               --
                               -- Depending on your system's characteristics,
                               -- using a too small value may generate audio glitches.
                               -- Hence, when in doubt, use 'usingAudioOutput' which uses
                               -- a safe default value.
                               -> m a
                               -> m (Either Text a)
usingAudioOutputWithMinLatency minLatency act =
  bracket bra ket $ either
    (const $ return $ Left "Audio output failed to initialize")
    (const $ fmap Right act)

 where

  -- TODO to enable very low (thus unsafe) latencies, override portaudio's min latency (use a 'Just' instead of 'Nothing')
  bra = liftIO $ initializeAudioOutput minLatency Nothing

  -- we ignore the initialization return because regardless of wether it succeeded or not,
  -- the 'initializeAudioOutput' call must be matched with a 'teardownAudioOutput' call.
  ket _ = liftIO teardownAudioOutput

initializeAudioOutput :: Time Duration System
                      -- ^ The audio output stream will have a latency no smaller than this value.
                      -> Maybe (Time Duration System)
                      -- ^ When the 'Just' value, floored to the previous millisecond,
                      -- is strictly positive, it is used to set the
                      -- <http://www.portaudio.com/docs/latency.html#portaudio PA_MIN_LATENCY_MSEC>
                      -- environment variable,
                      -- to override the Portaudio minimum latency. Use only if you know
                      -- your system can handle that latency, else, use 'Nothing'.
                      -> IO (Either () ())
initializeAudioOutput a b =
  bool (Left ()) (Right ()) <$>
    initializeAudioOutput_
      (realToFrac $ unsafeToSecs a)
      (maybe 0 (fromIntegral . toMicros) b)

-- | Should be called prior to using 'effect***' and 'midi***' functions.
foreign import ccall "initializeAudioOutput"
  initializeAudioOutput_ :: Float -> Int -> IO Bool

-- | Undoes what 'initializeAudioOutput' did.
foreign import ccall "teardownAudioOutput"
  teardownAudioOutput :: IO ()

-- | Plays a 'MusicalEvent'.
--
-- If a 'StopNote' is played less than @audio latency@ milliseconds after
-- its corresponding 'StartNote', the note won't be audible.
--
-- This function is thread-safe.
--
-- This function should be called from an action
-- run with 'usingAudioOutput' or 'usingAudioOutputWithMinLatency'.
-- If this is not the case, it hans no effect and returns 'False'.
play :: MusicalEvent
     -> IO Bool
     -- ^ 'True' if the call succeeds.
play (StartNote n@(InstrumentNote _ _ i) (NoteVelocity v)) = case i of
  SineSynthAHDSR e ahdsr -> midiNoteOnAHDSR (fromIntegral $ fromEnum e) ahdsr pitch vel
  Wind k -> effectOn (fromIntegral k) pitch vel
 where
  (MidiPitch pitch) = instrumentNoteToMidiPitch n
  vel = CFloat v
play (StopNote n@(InstrumentNote _ _ i)) = case i of
  SineSynthAHDSR e ahdsr -> midiNoteOffAHDSR (fromIntegral $ fromEnum e) ahdsr pitch
  Wind _ -> effectOff pitch
 where
  (MidiPitch pitch) = instrumentNoteToMidiPitch n

midiNoteOffAHDSR :: CInt -> AHDSR'Envelope -> CShort -> IO Bool
midiNoteOnAHDSR :: CInt -> AHDSR'Envelope -> CShort -> CFloat -> IO Bool
midiNoteOffAHDSR t (AHDSR'Envelope a h d r ai di ri s) i   =
  midiNoteOffAHDSR_ t (fromIntegral a) (interpolationToCInt ai) (fromIntegral h) (fromIntegral d) (interpolationToCInt di) (realToFrac s) (fromIntegral r) (interpolationToCInt ri) i
midiNoteOnAHDSR  t (AHDSR'Envelope a h d r ai di ri s) i v =
  midiNoteOnAHDSR_  t (fromIntegral a) (interpolationToCInt ai) (fromIntegral h) (fromIntegral d) (interpolationToCInt di) (realToFrac s) (fromIntegral r) (interpolationToCInt ri) i v

foreign import ccall "effectOn" effectOn :: CInt -> CShort -> CFloat -> IO Bool
foreign import ccall "effectOff" effectOff :: CShort -> IO Bool
foreign import ccall "midiNoteOnAHDSR_" midiNoteOnAHDSR_ :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt -> CShort -> CFloat -> IO Bool
foreign import ccall "midiNoteOffAHDSR_" midiNoteOffAHDSR_ :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt -> CShort -> IO Bool
