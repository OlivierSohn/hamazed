{-# LANGUAGE ForeignFunctionInterface #-}

module Imj.Audio
      ( -- * Using audio
        usingAudio
      -- * Midi Synth
      , midiNoteOn
      , midiNoteOff
      -- * Effect
      , effectOn
      , effectOff
      -- * reexports
      , CInt, CShort, CFloat
      ) where

import Foreign.C
import Control.Concurrent(threadDelay)
import Control.Monad.IO.Unlift(MonadUnliftIO, liftIO)
import UnliftIO.Exception(bracket)

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
