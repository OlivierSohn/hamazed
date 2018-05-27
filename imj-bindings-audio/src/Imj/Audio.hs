{-# LANGUAGE ForeignFunctionInterface #-}

module Imj.Audio
      ( -- * Using audio
        usingAudio
      -- * Midi Synth
      , midiNoteOn
      , midiNoteOff
      -- * Midi AHDSR Synth
      , midiNoteOnAHDSR
      , midiNoteOffAHDSR
      -- * Effect
      , effectOn
      , effectOff
      -- * reexports
      , module Imj.Music.CTypes
      , CInt, CShort, CFloat
      ) where

import Foreign.C
import Control.Concurrent(threadDelay)
import Control.Monad.IO.Unlift(MonadUnliftIO, liftIO)
import UnliftIO.Exception(bracket)

import Imj.Music.CTypes

foreign import ccall "effectOn" effectOn :: CInt -> CShort -> CFloat -> IO ()
foreign import ccall "effectOff" effectOff :: CShort -> IO ()

-- | Should be called prior to using any other function (see 'withAudio')
foreign import ccall "initializeAudio" initializeAudio :: IO Bool

-- | Fades-out all audio quikcly (within 'maxShutdownDurationMicros') and closes
-- any open audio channel.
foreign import ccall "stopAudioGracefully" stopAudioGracefully :: IO ()

-- | Stops audio abruptly (see 'withAudio').
foreign import ccall "teardownAudio" teardownAudio :: IO ()
foreign import ccall "midiNoteOn" midiNoteOn :: CInt -> CShort -> CFloat -> IO ()
foreign import ccall "midiNoteOff" midiNoteOff :: CInt -> CShort -> IO ()
foreign import ccall "midiNoteOnAHDSR_" midiNoteOnAHDSR_ :: CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CShort -> CFloat -> IO ()
foreign import ccall "midiNoteOffAHDSR_" midiNoteOffAHDSR_ :: CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CShort -> IO ()

midiNoteOffAHDSR :: CInt -> AHDSR -> CShort -> IO ()
midiNoteOffAHDSR t (AHDSR a h d r s) i = midiNoteOffAHDSR_ t (fromIntegral a) (fromIntegral h) (fromIntegral d) (realToFrac s) (fromIntegral r) i
midiNoteOnAHDSR :: CInt -> AHDSR -> CShort -> CFloat -> IO ()
midiNoteOnAHDSR t (AHDSR a h d r s) i v = midiNoteOnAHDSR_ t (fromIntegral a) (fromIntegral h) (fromIntegral d) (realToFrac s) (fromIntegral r) i v

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
