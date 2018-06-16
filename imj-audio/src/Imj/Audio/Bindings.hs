{-# LANGUAGE ForeignFunctionInterface #-}

module Imj.Audio.Bindings
      (
      -- * Init / Teardown
      initializeAudio
      , teardownAudio
      , maxShutdownDurationMicros
      , stopAudioGracefully
      -- * Start / stop notes
      , midiNoteOn
      , midiNoteOff
      , midiNoteOnAHDSR
      , midiNoteOffAHDSR
      , effectOn
      , effectOff
      -- * Analyze
      , analyzeAHDSREnvelope_
      -- * Reexports
      , module Imj.Music.CTypes
      , CInt, CShort, CFloat
      ) where

import           Foreign.C
import           Foreign.Ptr

import Imj.Music.CTypes

foreign import ccall "effectOn" effectOn :: CInt -> CShort -> CFloat -> IO ()
foreign import ccall "effectOff" effectOff :: CShort -> IO ()

-- | Should be called prior to using any other function (see 'withAudio')
foreign import ccall "initializeAudio" initializeAudio :: Int
                                                       -- ^ Latency, in milliseconds. If the value is strictly positive,
                                                       -- the environment variable PA_MIN_LATENCY_MSEC will be set accordingly.
                                                       -- Pass 0 or a negative value to not set this variable. See <http://www.portaudio.com/docs/latency.html the doc on this subject>.
                                                       -> Float
                                                       -- ^ Minimum latency, in seconds. This value will be used to configure the
                                                       -- audio output strem.
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

midiNoteOffAHDSR :: CInt -> AHDSR -> CShort -> IO ()
midiNoteOnAHDSR :: CInt -> AHDSR -> CShort -> CFloat -> IO ()
midiNoteOffAHDSR t (AHDSR a h d r ai di ri s) i   =
  midiNoteOffAHDSR_ t (fromIntegral a) (fromIntegral $ itpToInt ai) (fromIntegral h) (fromIntegral d) (fromIntegral $ itpToInt di) (realToFrac s) (fromIntegral r) (fromIntegral $ itpToInt ri) i
midiNoteOnAHDSR  t (AHDSR a h d r ai di ri s) i v =
  midiNoteOnAHDSR_  t (fromIntegral a) (fromIntegral $ itpToInt ai) (fromIntegral h) (fromIntegral d) (fromIntegral $ itpToInt di) (realToFrac s) (fromIntegral r) (fromIntegral $ itpToInt ri) i v


-- | An upperbound on the time it will take for audio to shutdown gracefully (see 'stopAudioGracefully').
maxShutdownDurationMicros :: Int
maxShutdownDurationMicros = 1000*12
