{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

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

-}

module Imj.Audio.Output
      (
      -- * Bracketed init / teardown
        usingAudioOutput
      , usingAudioOutputWithMinLatency
      -- * Playing music
      , MusicalEvent(..)
      , play
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

import           Control.Concurrent(threadDelay)
import           Control.Monad.IO.Unlift(MonadUnliftIO, liftIO)
import           Data.Bool(bool)
import           Foreign.C
import           UnliftIO.Exception(bracket)

import           Imj.Audio.Envelope
import           Imj.Music.Instruction
import           Imj.Music.Instrument
import           Imj.Timing


-- | Initializes the audio output stream,
-- runs the action containing (possibly concurrent) calls to 'play'.
--
-- When the action finishes, the audio output signal is swiftly cross-faded to zero,
-- and the audio output stream shutdown.
--
-- Re-entrancy is not supported.
usingAudioOutput :: MonadUnliftIO m
                 => m a
                 -> m a
usingAudioOutput = usingAudioOutputWithMinLatency $ fromSecs 0.008

-- | Re-entrancy is not supported.
usingAudioOutputWithMinLatency :: MonadUnliftIO m
                               => Time Duration System
                               -- ^ The minimum latency of the audio output stream.
                               --
                               -- Depending on your system's characteristics,
                               -- using a too small value may generate audio glitches.
                               -- Hence, when in doubt, use 'usingAudioOutput' which uses
                               -- a safe default value.
                               -> m a
                               -> m a
usingAudioOutputWithMinLatency minLatency act =
  bracket bra ket $ either
    (\_ -> fail "audio failed to initialize")
    (\_ -> act)

 where

  bra =
    liftIO $ initializeAudioOutput minLatency Nothing  -- TODO to enable very low latencies, we could override portaudio's min latency
      >>= either (return . Left)
        (\res -> do
          threadDelay 1000000 -- wait some time (on my osx system, this time is necessary
                              -- to be able to play sound)
          return $ Right res)

  ket _ = liftIO $ do
    stopAudioOutputGracefully
    threadDelay maxShutdownDurationMicros
    teardownAudioOutput

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

-- | Should be called prior to using any other function.
foreign import ccall "initializeAudioOutput"
  initializeAudioOutput_ :: Float -> Int -> IO Bool

-- | Fades-out all audio quickly (within 'maxShutdownDurationMicros') and closes
-- any open audio channel.
foreign import ccall "stopAudioOutputGracefully" stopAudioOutputGracefully :: IO ()
-- | Stops audio abruptly (see 'usingAudioOutput').
foreign import ccall "teardownAudioOutput" teardownAudioOutput :: IO ()
-- | An upperbound on the time it will take for audio to shutdown gracefully (see 'stopAudioOutputGracefully').
maxShutdownDurationMicros :: Int
maxShutdownDurationMicros = 1000*12

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
  SineSynth ect -> midiNoteOn (fromIntegral $ unEnvelopeCharacteristicTime ect) pitch vel
  Wind k -> effectOn (fromIntegral k) pitch vel
 where
  (MidiPitch pitch) = instrumentNoteToMidiPitch n
  vel = CFloat v
play (StopNote n@(InstrumentNote _ _ i)) = case i of
  SineSynthAHDSR e ahdsr -> midiNoteOffAHDSR (fromIntegral $ fromEnum e) ahdsr pitch
  SineSynth ect -> midiNoteOff (fromIntegral $ unEnvelopeCharacteristicTime ect) pitch
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
foreign import ccall "midiNoteOn" midiNoteOn :: CInt -> CShort -> CFloat -> IO Bool
foreign import ccall "midiNoteOff" midiNoteOff :: CInt -> CShort -> IO Bool
foreign import ccall "midiNoteOnAHDSR_" midiNoteOnAHDSR_ :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt -> CShort -> CFloat -> IO Bool
foreign import ccall "midiNoteOffAHDSR_" midiNoteOffAHDSR_ :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt -> CShort -> IO Bool
