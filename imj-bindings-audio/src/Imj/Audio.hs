{-# LANGUAGE ForeignFunctionInterface #-}

{- |

== Instrument

In this documentation, an /instrument/ is a /combination/ of parameters passed to
'midiNoteOn', 'midiNoteOff', 'midiNoteOnAHDSR' and 'midiNoteOffAHDSR'.

== Overview

The functions exported by this module let you play polyphonic, poly-instrumental music and sounds,
in real time.

These functions can be called /concurrently/ from /any/ Haskell thread (bounded or not).

'usingAudio' initializes the audio environment that will be used to:

* play notes with a simple instrument ('midiNoteOn', 'midiNoteOff')
* play notes with an envelope-based instrument ('midiNoteOnAHDSR', 'midiNoteOffAHDSR')
* play a time-varying sound based on filtered white noise ('effectOn', 'effectOff')

All these functions should only be called from within an action run in a 'usingAudio'.

== C++ Audio engine

Behind the scenes,
<https://github.com/OlivierSohn/cpp.audio/tree/master/include this C++ audio engine>
is used, through
<https://github.com/OlivierSohn/hamazed/tree/master/imj-bindings-audio these bindings>.

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
      (
        -- * Bracketted audio initialization
        usingAudio
        -- * Playing an instrument
      , midiNoteOn
      , midiNoteOff
      , midiNoteOnAHDSR
      , midiNoteOffAHDSR
        -- * Playing an effect
      , effectOn
      , effectOff
      -- * Analyze envelopes
      -- | 'analyzeAHDSREnvelope' gives you the exact shape of the envelope
      -- of an instrument. It can be usefull to give a visual feedback to users.
      , analyzeAHDSREnvelope
      -- * Reexports
      , module Imj.Music.CTypes
      , CInt, CShort, CFloat
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

import Imj.Music.CTypes

foreign import ccall "effectOn" effectOn :: CInt -> CShort -> CFloat -> IO ()
foreign import ccall "effectOff" effectOff :: CShort -> IO ()

-- | Should be called prior to using any other function (see 'withAudio')
foreign import ccall "initializeAudio" initializeAudio :: Int
                                                       -- ^ Latency, in milliseconds. If the value is strictly positive,
                                                       -- the environment variable PA_MIN_LATENCY_MSEC will be set accordingly.
                                                       -- Pass 0 or a negative value to not set this variable. See <http://www.portaudio.com/docs/latency.html the doc on this subject>.
                                                       -> IO Bool

-- | Fades-out all audio quikcly (within 'maxShutdownDurationMicros') and closes
-- any open audio channel.
foreign import ccall "stopAudioGracefully" stopAudioGracefully :: IO ()

-- | Stops audio abruptly (see 'withAudio').
foreign import ccall "teardownAudio" teardownAudio :: IO ()
foreign import ccall "midiNoteOn" midiNoteOn :: CInt -> CShort -> CFloat -> IO ()
foreign import ccall "midiNoteOff" midiNoteOff :: CInt -> CShort -> IO ()
foreign import ccall "midiNoteOnAHDSR_" midiNoteOnAHDSR_ :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt -> CShort -> CFloat -> IO ()
foreign import ccall "midiNoteOffAHDSR_" midiNoteOffAHDSR_ :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt -> CShort -> IO ()
foreign import ccall "analyzeAHDSREnvelope_" analyzeAHDSREnvelope_ :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr CFloat)

-- https://stackoverflow.com/questions/43372363/releasing-memory-allocated-by-c-runtime-from-haskell
foreign import ccall "stdlib.h free" c_free :: Ptr CFloat -> IO ()

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

midiNoteOffAHDSR :: CInt -> AHDSR -> CShort -> IO ()
midiNoteOnAHDSR :: CInt -> AHDSR -> CShort -> CFloat -> IO ()
midiNoteOffAHDSR t (AHDSR a h d r ai di ri s) i   =
  midiNoteOffAHDSR_ t (fromIntegral a) (fromIntegral $ itpToInt ai) (fromIntegral h) (fromIntegral d) (fromIntegral $ itpToInt di) (realToFrac s) (fromIntegral r) (fromIntegral $ itpToInt ri) i
midiNoteOnAHDSR  t (AHDSR a h d r ai di ri s) i v =
  midiNoteOnAHDSR_  t (fromIntegral a) (fromIntegral $ itpToInt ai) (fromIntegral h) (fromIntegral d) (fromIntegral $ itpToInt di) (realToFrac s) (fromIntegral r) (fromIntegral $ itpToInt ri) i v


-- | Initializes audio, runs the action, shutdowns audio (gracefully, with cross-fades) and returns.
--
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

  bra = liftIO $ initializeAudio (-1) -- using the default latency

  ket _ = liftIO $ do
    stopAudioGracefully
    threadDelay maxShutdownDurationMicros
    teardownAudio

-- | An upperbound on the time it will take for audio to shutdown gracefully (see 'stopAudioGracefully').
maxShutdownDurationMicros :: Int
maxShutdownDurationMicros = 1000*12
