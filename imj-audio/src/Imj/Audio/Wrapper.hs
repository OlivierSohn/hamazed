{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Audio.Wrapper
      ( -- * Using the audio engine
      {-|
<https://github.com/OlivierSohn/cpp.audio/tree/master/include This C++ audio engine>
is used, through
<https://github.com/OlivierSohn/hamazed/tree/master/imj-audio these bindings>, and
the <http://www.portaudio.com/ portaudio> c library is used to talk to your audio system.

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
      usingAudio
        -- * Playing music
      , play
      , MusicalEvent(..)
      -- * Analysing envelopes
      , envelopeShape

      ) where

import           Control.Concurrent(threadDelay)
import           Control.Monad.IO.Unlift(MonadUnliftIO, liftIO)
import           Data.Vector.Unboxed(Vector, unsafeFreeze)
import           Data.Vector.Unboxed.Mutable(new, unsafeWrite)
import           Foreign.C
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           UnliftIO.Exception(bracket)

import           Imj.Audio.Bindings


-- | Initializes the audio context, runs the action, shutdowns the audio context by
-- driving the audio signal smoothly to zero, and returns when there is no more audio played.
--
-- Because of the unicity of the audio context,
-- at most one call to 'usingAudio' should be active in the program at any time.
--
-- All functions calling 'play' should be called from within the action run by 'usingAudio'.
usingAudio :: MonadUnliftIO m
           => m a
           -- ^ The action to run.
           -- This action should contain no call to 'usingAudio'.
           -> m a
usingAudio act =

  bracket bra ket $ \initialized ->
    if initialized
      then
        act
      else
        fail "audio failed to initialize"

 where

  bra = liftIO $ do
    res <- initializeAudio (-1) -- using the default latency
    threadDelay 1000000 -- wait some time (on my osx system, this time is necessary
                        -- to be able to play sound)
    return res

  ket _ = liftIO $ do
    stopAudioGracefully
    threadDelay maxShutdownDurationMicros
    teardownAudio

-- | Plays a 'MusicalEvent'. It is safe to call this function concurrently
-- from multiple threads.
play :: MusicalEvent -> IO ()
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


-- | Returns lists of consecutive envelope values.
--
-- If the 'Instrument' uses an
-- 'Envelope' 'AutoRelease', a single list is returned, covering all envelope phases, from attack to release.
--
-- If the 'Instrument' uses an
-- 'Envelope' 'KeyRelease', two lists are returned:
--
-- * The first list covers phases from attack to the beginning of sustain.
-- * The second list covers the end of sustain to the release phase.
envelopeShape :: Instrument -> IO [Vector Float]
envelopeShape = \case
  SineSynthAHDSR e ahdsr -> analyzeAHDSREnvelope (fromIntegral $ fromEnum e) ahdsr
  SineSynth _ -> return []
  Wind _ -> return []

analyzeAHDSREnvelope :: CInt
                     -> AHDSR
                     -> IO [Vector Float]
analyzeAHDSREnvelope t (AHDSR a h d r ai di ri s) =
  alloca $ \ptrNElems -> alloca $ \ptrSplitAt -> do
    buf <- analyzeAHDSREnvelope_ t (fromIntegral a) (fromIntegral $ itpToInt ai) (fromIntegral h) (fromIntegral d) (fromIntegral $ itpToInt di) (realToFrac s) (fromIntegral r) (fromIntegral $ itpToInt ri) ptrNElems ptrSplitAt
    nElems <- fromIntegral <$> peek ptrNElems
    split <- fromIntegral <$> peek ptrSplitAt
    let slices =
          if split < 0
            then
              [(0,nElems-1)
              ]
            else
              [(0,split-1)
              ,(split,nElems-1)
              ]
    res <- mapM (uncurry $ takeBuffer buf) slices
    c_free buf
    return res
   where
    takeBuffer buf iStart iEnd = do
      uv <- new (1 + iEnd - iStart)
      mapM_
        (\i -> do
          val <- (peek $ plusPtr buf $ i * (sizeOf (undefined :: CFloat))) :: IO CFloat
          unsafeWrite uv (i-iStart) $ realToFrac val)
        [iStart..iEnd]
      unsafeFreeze uv

-- https://stackoverflow.com/questions/43372363/releasing-memory-allocated-by-c-runtime-from-haskell
foreign import ccall "stdlib.h free" c_free :: Ptr CFloat -> IO ()
