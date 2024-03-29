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
      -- * Avoiding MIDI jitter
      , setMaxMIDIJitter
      -- * Playing music
      , play
      , MusicalEvent(..)
      -- * Postprocessing
      , getReverbInfo
      , useReverb
      , setReverbWetRatio
      -- * C++ audio-engine implementation details
      {-|

      === Design evolution, from lockfull to lockfree

      To synchronize accesses to shared data between
      the audio realtime thread and non realtime threads, a locking approach was first used.

      ==== Lockfull

      This approach, while being easy to implement and reason about, lead to
      <https://en.wikipedia.org/wiki/Priority_inversion priority inversion> issues:
      sometimes, the realtime thread would have to wait (a too-long time)
      for a non-realtime thread to release the lock. This caused
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
import qualified Data.Vector.Storable as S
import           Foreign.C(CInt(..), CULLong(..), CShort(..), CFloat(..), CDouble(..), CString, withCString)
import           Foreign.ForeignPtr(withForeignPtr)
import           Foreign.Marshal.Alloc
import           Foreign.Ptr(Ptr, nullPtr)
import           Foreign.Storable
import           UnliftIO.Exception(bracket)

import           Imj.Audio.Envelope
import           Imj.Audio.Harmonics
import           Imj.Audio.Midi
import           Imj.Audio.SampleRate
import           Imj.Audio.SpaceResponse
import           Imj.Data.AlmostFloat
import           Imj.Music.Instruction
import           Imj.Music.Instrument
import           Imj.Music.Score(VoiceId(..))
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
usingAudioOutput = usingAudioOutputWithMinLatency globalSampleRate $ fromSecs 0.008
-- if you hear audio cracks, use a larger latency

-- | Same as 'usingAudioOutput' except that the minimum latency can be configured.
--
-- Note that the latency parameter will be used only if there is currently no other active call to
-- 'usingAudioOutput' or 'usingAudioOutputWithMinLatency', else it is ignored (in that case,
-- the global audio output stream is already initialized).
usingAudioOutputWithMinLatency :: MonadUnliftIO m
                               => Int
                               -- ^ Sampling rate
                               ->Time Duration System
                               -- ^ The minimum latency of the audio output stream.
                               --
                               -- Depending on your system's characteristics,
                               -- using a too small value may generate audio glitches.
                               -- Hence, when in doubt, use 'usingAudioOutput' which uses
                               -- a safe default value.
                               -> m a
                               -> m (Either Text a)
usingAudioOutputWithMinLatency samplingRate minLatency act =
  bracket bra ket $ either
    (const $ return $ Left "Audio output failed to initialize")
    (const $ fmap Right act)

 where

  -- TODO to enable very low (thus unsafe) latencies, override portaudio's min latency (use a 'Just' instead of 'Nothing')
  bra = liftIO $ initializeAudioOutput samplingRate minLatency Nothing

  -- we ignore the initialization return because regardless of wether it succeeded or not,
  -- the 'initializeAudioOutput' call must be matched with a 'teardownAudioOutput' call.
  ket _ = liftIO teardownAudioOutput

initializeAudioOutput :: Int
                      -- ^ Sampling rate
                      -> Time Duration System
                      -- ^ The audio output stream will have a latency no smaller than this value.
                      -> Maybe (Time Duration System)
                      -- ^ When the 'Just' value, floored to the previous millisecond,
                      -- is strictly positive, it is used to set the
                      -- <http://www.portaudio.com/docs/latency.html#portaudio PA_MIN_LATENCY_MSEC>
                      -- environment variable,
                      -- to override the Portaudio minimum latency. Use only if you know
                      -- your system can handle that latency, else, use 'Nothing'.
                      -> IO (Either () ())
initializeAudioOutput sampling_rate a b =
  bool (Left ()) (Right ()) <$>
    initializeAudioOutput_
      sampling_rate
      (realToFrac $ unsafeToSecs a)
      (maybe 0 (fromIntegral . toMicros) b)

-- | Should be called prior to using 'effect***' and 'midi***' functions.
foreign import ccall "initializeAudioOutput"
  initializeAudioOutput_ :: Int -> Float -> Int -> IO Bool

-- | Undoes what 'initializeAudioOutput' did.
foreign import ccall "teardownAudioOutput"
  teardownAudioOutput :: IO ()


foreign import ccall "setMaxMIDIJitter"
  setMaxMIDIJitter_ :: CULLong -> IO ()

-- TODO should this be per-source? every source can have a different polling setting,
-- and different connection characteristics with the server.
setMaxMIDIJitter :: MaxMIDIJitter -> IO ()
setMaxMIDIJitter = setMaxMIDIJitter_ . (*1000) . fromIntegral

-- | Plays a 'MusicalEvent'.
--
-- If a 'StopNote' is played less than @audio latency@ milliseconds after
-- its corresponding 'StartNote', the note won't be audible.
--
-- This function is thread-safe.
--
-- This function should be called from an action
-- run with 'usingAudioOutput' or 'usingAudioOutputWithMinLatency'.
-- If this is not the case, it has no effect and returns 'Left ()'.
play :: MusicalEvent Instrument
     -> VoiceId
     -- ^ Internally, each "instrument" has a separate domain for note ids.
     -- But when the same instrument is used to play different voices at the same time
     -- the same noteon for the same pitch could be issued several times.
     -- VoiceId will be used to associate a noteoff or notechange ot a previous noteon.
     -> IO (Either () ())
play (StartNote mayMidi n@(InstrumentNote _ _ i) (NoteVelocity v) (NotePan pan)) (VoiceId voice) = bool (Left ()) (Right ()) <$> case i of
  Synth (Oscillations osc har) e ahdsr ->
    let (harPtr, harSz) = S.unsafeToForeignPtr0 $ unHarmonics har
    in withForeignPtr harPtr $ \harmonicsPtr ->
         midiNoteOnAHDSR (Right osc) e ahdsr (harmonicsPtr, harSz) pitch vel mayMidi panF voiceI
  Synth Noise e ahdsr -> midiNoteOnAHDSR (Left $ -1) e ahdsr (nullPtr, 0) pitch vel mayMidi panF voiceI
  Synth (Sweep sweep_duration freq freqType itp) e ahdsr -> midiNoteOnAHDSRSweep (Left $ -2) e ahdsr (nullPtr, 0) sweep_duration freq freqType itp pitch vel mayMidi panF voiceI
  Wind k -> effectOn voiceI (fromIntegral k) pitch vel panF
 where
  (MidiPitch pitch) = instrumentNoteToMidiPitch n
  vel = CFloat v
  panF = CFloat pan
  voiceI = fromIntegral voice
play (StopNote mayMidi n@(InstrumentNote _ _ i)) (VoiceId voice) = bool (Left ()) (Right ()) <$> case i of
  Synth (Oscillations osc har) e ahdsr ->
    let (harPtr, harSz) = S.unsafeToForeignPtr0 $ unHarmonics har
    in withForeignPtr harPtr $ \harmonicsPtr ->
         midiNoteOffAHDSR (Right osc) e ahdsr (harmonicsPtr, harSz) pitch mayMidi voiceI
  Synth Noise e ahdsr -> midiNoteOffAHDSR (Left $ -1) e ahdsr (nullPtr, 0) pitch mayMidi voiceI
  Synth (Sweep sweep_duration freq freqType itp) e ahdsr -> midiNoteOffAHDSRSweep (Left $ -2) e ahdsr (nullPtr, 0) sweep_duration freq freqType itp pitch mayMidi voiceI
  Wind _ -> effectOff voiceI pitch
 where
  (MidiPitch pitch) = instrumentNoteToMidiPitch n
  voiceI = fromIntegral voice

midiNoteOffAHDSR ::
  Either Int Oscillator -> ReleaseMode -> AHDSR'Envelope -> (Ptr HarmonicProperties, Int) -> CShort -> Maybe MidiInfo -> CInt -> IO Bool
midiNoteOnAHDSR ::
  Either Int Oscillator -> ReleaseMode -> AHDSR'Envelope -> (Ptr HarmonicProperties, Int) -> CShort -> CFloat -> Maybe MidiInfo -> CFloat -> CInt -> IO Bool
midiNoteOffAHDSRSweep ::
  Either Int Oscillator -> ReleaseMode -> AHDSR'Envelope -> (Ptr HarmonicProperties, Int) -> Int -> AlmostFloat -> SweepFreqType -> Interpolation -> CShort -> Maybe MidiInfo -> CInt -> IO Bool
midiNoteOnAHDSRSweep ::
  Either Int Oscillator -> ReleaseMode -> AHDSR'Envelope -> (Ptr HarmonicProperties, Int) -> Int -> AlmostFloat -> SweepFreqType -> Interpolation -> CShort -> CFloat -> Maybe MidiInfo -> CFloat -> CInt -> IO Bool
midiNoteOffAHDSR osc t (AHDSR'Envelope a h d r ai di ri s) (harmonicsPtr, harmonicsSz) i mayMidi voice =
    midiNoteOffAHDSR_
      voice
      (either fromIntegral (fromIntegral . fromEnum) osc)
      (fromIntegral $ fromEnum t)
      (fromIntegral a)
      (interpolationToCInt ai)
      (fromIntegral h)
      (fromIntegral d)
      (interpolationToCInt di)
      (realToFrac s)
      (fromIntegral r)
      (interpolationToCInt ri)
      harmonicsPtr
      (fromIntegral harmonicsSz)
      i
      src
      time
 where
  (src, time) = mayMidiInfoToSrcTime mayMidi
midiNoteOffAHDSRSweep osc t (AHDSR'Envelope a h d r ai di ri s) (harmonicsPtr, harmonicsSz) sweep_duration sweep_freq sweep_freq_type sweep_interp i mayMidi voice =
    midiNoteOffAHDSRSweep_
      voice
      (either fromIntegral (fromIntegral . fromEnum) osc)
      (fromIntegral $ fromEnum t)
      (fromIntegral a)
      (interpolationToCInt ai)
      (fromIntegral h)
      (fromIntegral d)
      (interpolationToCInt di)
      (realToFrac s)
      (fromIntegral r)
      (interpolationToCInt ri)
      harmonicsPtr
      (fromIntegral harmonicsSz)
      (fromIntegral sweep_duration)
      (realToFrac $ unAlmostFloat sweep_freq)
      (fromIntegral $ fromEnum sweep_freq_type)
      (interpolationToCInt sweep_interp)
      i
      src
      time
 where
  (src, time) = mayMidiInfoToSrcTime mayMidi
midiNoteOnAHDSR osc t (AHDSR'Envelope a h d r ai di ri s) (harmonicsPtr, harmonicsSz) i v mayMidi pan voice =
    midiNoteOnAHDSR_
      voice
      pan
      (either fromIntegral (fromIntegral . fromEnum) osc)
      (fromIntegral $ fromEnum t)
      (fromIntegral a)
      (interpolationToCInt ai)
      (fromIntegral h)
      (fromIntegral d)
      (interpolationToCInt di)
      (realToFrac s)
      (fromIntegral r)
      (interpolationToCInt ri)
      harmonicsPtr
      (fromIntegral harmonicsSz)
      i
      v
      src
      time
 where
  (src, time) = mayMidiInfoToSrcTime mayMidi
midiNoteOnAHDSRSweep osc t (AHDSR'Envelope a h d r ai di ri s) (harmonicsPtr, harmonicsSz) sweep_duration sweep_freq sweep_freq_type sweep_interp i v mayMidi pan voice =
    midiNoteOnAHDSRSweep_
      voice
      pan
      (either fromIntegral (fromIntegral . fromEnum) osc)
      (fromIntegral $ fromEnum t)
      (fromIntegral a)
      (interpolationToCInt ai)
      (fromIntegral h)
      (fromIntegral d)
      (interpolationToCInt di)
      (realToFrac s)
      (fromIntegral r)
      (interpolationToCInt ri)
      harmonicsPtr
      (fromIntegral harmonicsSz)
      (fromIntegral sweep_duration)
      (realToFrac $ unAlmostFloat sweep_freq)
      (fromIntegral $ fromEnum sweep_freq_type)
      (interpolationToCInt sweep_interp)
      i
      v
      src
      time
 where
  (src, time) = mayMidiInfoToSrcTime mayMidi

mayMidiInfoToSrcTime :: Maybe MidiInfo -> (CInt, CULLong)
mayMidiInfoToSrcTime mayMidi = (src, time)
 where
  -- -1 encodes "no source"
  src  = fromIntegral $ maybe (-1 :: CInt) (fromIntegral . unMidiSourceIdx . source) mayMidi
  time = fromIntegral $ maybe 0 timestamp mayMidi

foreign import ccall "getConvolutionReverbSignature_" getReverbSignature :: CString -> CString -> Ptr SpaceResponse -> IO Bool

getReverbInfo :: String -> String -> IO (Maybe SpaceResponse)
getReverbInfo dirName fileName =
  withCString dirName $ \d -> withCString fileName $ \f -> alloca $ \p ->
    getReverbSignature d f p >>= bool
      (return Nothing)
      (Just <$> peek p)

foreign import ccall "dontUseReverb_" dontUseReverb_ :: IO Bool
foreign import ccall "useReverb_" useReverb_ :: CString -> CString -> CDouble -> IO Bool
useReverb :: Double -> Maybe (String, String) -> IO (Either () ())
useReverb wet =
  fmap (bool (Left ()) (Right ())) .
    maybe
      dontUseReverb_
      (\(dirName, fileName) ->
        withCString dirName $ \d -> withCString fileName $ \f -> useReverb_ d f (realToFrac wet))

foreign import ccall "setReverbWetRatio" setReverbWetRatio_ :: CDouble -> IO Bool
setReverbWetRatio :: Double -> IO (Either () ())
setReverbWetRatio =
  fmap (bool (Left ()) (Right ())) .
    setReverbWetRatio_ . realToFrac

foreign import ccall "effectOn" effectOn :: CInt
                                         -- ^ voice
                                         -> CInt -> CShort -> CFloat -> CFloat -> IO Bool
foreign import ccall "effectOff" effectOff :: CInt
                                         -- ^ voice
                                           -> CShort -> IO Bool
foreign import ccall "midiNoteOnAHDSR_"
  midiNoteOnAHDSR_ :: CInt
                   -- ^ Voice
                   -> CFloat
                   -- ^ Stereo
                   -> CInt -> CInt
                   -- ^ Envelope type
                   -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt
                   -> Ptr HarmonicProperties -> CInt
                   -> CShort -> CFloat
                   -> CInt -> CULLong
                   -> IO Bool
foreign import ccall "midiNoteOffAHDSR_"
  midiNoteOffAHDSR_ :: CInt
                    -- ^ Voice
                    -> CInt -> CInt
                    -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt
                    -> Ptr HarmonicProperties -> CInt
                    -> CShort
                    -> CInt -> CULLong
                    -> IO Bool

foreign import ccall "midiNoteOnAHDSRSweep_"
  midiNoteOnAHDSRSweep_ :: CInt
                        -- ^ Voice
                        -> CFloat
                        -- ^ Stereo
                        -> CInt -> CInt
                        -- ^ Envelope type
                        -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt
                        -> Ptr HarmonicProperties -> CInt
                        -> CInt
                        -- ^ Sweep duration
                        -> CFloat
                        -- ^ Sweep freq
                         -> CInt
                         -- ^ Sweep freq type
                         -> CInt
                         -- ^ Sweep interpolation
                        -> CShort -> CFloat
                        -> CInt -> CULLong
                        -> IO Bool
foreign import ccall "midiNoteOffAHDSRSweep_"
  midiNoteOffAHDSRSweep_ :: CInt
                         -- ^ Voice
                         -> CInt -> CInt
                         -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt
                         -> Ptr HarmonicProperties -> CInt
                         -> CInt
                         -- ^ Sweep duration
                         -> CFloat
                         -- ^ Sweep freq
                         -> CInt
                         -- ^ Sweep freq type
                         -> CInt
                         -- ^ Sweep interpolation
                         -> CShort
                         -> CInt -> CULLong
                         -> IO Bool
