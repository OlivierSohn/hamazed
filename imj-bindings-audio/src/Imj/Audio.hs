{-# LANGUAGE ForeignFunctionInterface #-}

{- |
      This module lets you play polyphonic, poly-instrumental music and sounds,
      in real time.

      Behind the scenes, it uses
      <https://github.com/OlivierSohn/cpp.audio/tree/master/include a custom C++ audio engine> through
      <https://github.com/OlivierSohn/hamazed/tree/master/imj-bindings-audio these bindings>.

      The produced audio signal will be written to your audio system through the
      cross-platform (Windows, OSX, Unix) <http://www.portaudio.com/ portaudio API>.

      * C++ objects representing instruments are automatically recycled,
      hence the RAM usage is proportional to the number of /active/ instruments at any given time.

      * The real-time audio thread is /not/ managed by the GHC runtime, so there will be
      /no/ audio pauses during GHC garbage collection.

      * In the C++ audio engine, we control thread priorities so that when taking the
      global audio lock, the thread that takes it has the realtime priority, to avoid priority inversion
      when the realtime audio thread waits for the lock. This means that on linux,
      you should ideally 'sudo' the command starting the program, so that
      the program has the necessary privileges to configure thread priorities.
      If you don't, a warning message will be logged in the console, and the program will run,
      but with less guarantees about audio "smoothness" because we can potentially have priority
      inversion effect.

      * In theory (it has not beed much tested yet) it should be safe to call
      functions in a concurent / multithreaded context, because the library
      underneath uses locks to protect accesses to shared data.
      * There is no restriction on the kind of thread (bounded / unbounded) you can call these functions from.

-}

module Imj.Audio
      ( -- * Output audio
      {- | 'usingAudio' initializes the audio environment that will be used to:

      * play notes with a simple instrument ('midiNoteOn', 'midiNoteOff')
      * play notes with an envelope-based instrument ('midiNoteOnAHDSR', 'midiNoteOffAHDSR')
      * play a time-varying sound based on filtered white noise ('effectOn', 'effectOff')
       -}
        usingAudio
      , midiNoteOn
      , midiNoteOff
      , midiNoteOnAHDSR
      , midiNoteOffAHDSR
      , effectOn
      , effectOff
      -- * Analyze envelopes
      -- | 'analyzeAHDSREnvelope' gives you the exact shape of the envelope
      -- of an instrument. It can be usefull to give a visual feedback to users.
      , analyzeAHDSREnvelope
      -- * reexports
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
foreign import ccall "initializeAudio" initializeAudio :: IO Bool

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

-- | Initializes audio, runs the action, shutdowns audio gracefully and waits
-- until audio is shutdown completely before returning.
usingAudio :: MonadUnliftIO m => m a -> m a
usingAudio act =

  bracket bra ket $ \initialized ->
    if initialized
      then
        act
      else
        fail "audio failed to initialize"

 where

  bra = liftIO initializeAudio

  ket _ = liftIO $ do
    stopAudioGracefully
    threadDelay maxShutdownDurationMicros
    teardownAudio

-- | An upperbound on the time it will take for audio to shutdown gracefully (see 'stopAudioGracefully').
maxShutdownDurationMicros :: Int
maxShutdownDurationMicros = 1000*12
