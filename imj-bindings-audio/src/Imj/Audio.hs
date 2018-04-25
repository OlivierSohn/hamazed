{-# LANGUAGE ForeignFunctionInterface #-}

module Imj.Audio
      ( -- * Using audio
        withAudio
      -- * Midi
      , midiNoteOn
      , midiNoteOff
      -- * Troubleshooting
      , beep
        ) where

import Foreign.C
import Control.Concurrent(threadDelay)
import Control.Exception(bracket)

-- | A test function to emit a beep sound.
foreign import ccall "beep" beep :: IO ()

-- | Should be called prior to using any other function (see 'withAudio')
foreign import ccall "initializeAudio" initializeAudio :: IO ()

-- | Should be called to shutdown audio gracefully, i.e fade-out quikcly and close
-- any open audio channel (see 'withAudio')
foreign import ccall "stopAudioGracefully" stopAudioGracefully :: IO ()

-- | Stops audio abruptly (see 'withAudio').
foreign import ccall "teardownAudio" teardownAudio :: IO ()
foreign import ccall "midiNoteOn" midiNoteOn :: CInt -> CFloat -> IO ()
foreign import ccall "midiNoteOff" midiNoteOff :: CInt -> IO ()

-- | Initializes audio, runs the action, shutdowns audio gracefully and waits
-- until audio is shutdown completely before returning.
withAudio :: IO () -> IO ()
withAudio = do

  bracket bra ket . const

 where

  bra = initializeAudio

  ket _ = do
    stopAudioGracefully -- takes max. 500 samples * 1/44100 secs < 12 ms
    threadDelay maxShutdownDurationMicros
    teardownAudio -- stops audio immediately.

-- | An upperbound on the time it will take for audio to shutdown gracefully (see 'stopAudioGracefully').
maxShutdownDurationMicros :: Int
maxShutdownDurationMicros = 1000*12
