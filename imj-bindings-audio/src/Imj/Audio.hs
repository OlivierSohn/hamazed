{-# LANGUAGE ForeignFunctionInterface #-}

module Imj.Audio
      ( -- * Using audio
        withAudio
      -- * Midi Synth
      , midiNoteOn
      , midiNoteOff
      -- * Effect
      , effectOn
      , effectOff
      -- * Test functions
      , beep
      -- * reexports
      , CInt, CShort, CFloat
      ) where

import Foreign.C
import Control.Concurrent(threadDelay)
import Control.Exception(bracket)

-- | A test function to emit a beep sound.
foreign import ccall "beep" beep :: IO ()

foreign import ccall "effectOn" effectOn :: CInt -> CShort -> CFloat -> IO ()
foreign import ccall "effectOff" effectOff :: CShort -> IO ()

-- | Should be called prior to using any other function (see 'withAudio')
foreign import ccall "initializeAudio" initializeAudio :: IO ()

-- | Fades-out all audio quikcly (within 'maxShutdownDurationMicros') and closes
-- any open audio channel.
foreign import ccall "stopAudioGracefully" stopAudioGracefully :: IO ()

-- | Stops audio abruptly (see 'withAudio').
foreign import ccall "teardownAudio" teardownAudio :: IO ()
foreign import ccall "midiNoteOn" midiNoteOn :: CShort -> CFloat -> IO ()
foreign import ccall "midiNoteOff" midiNoteOff :: CShort -> IO ()

-- | Initializes audio, runs the action, shutdowns audio gracefully and waits
-- until audio is shutdown completely before returning.
withAudio :: IO a -> IO a
withAudio =

  bracket bra ket . const

 where

  bra = initializeAudio

  ket _ = do
    stopAudioGracefully
    threadDelay maxShutdownDurationMicros
    teardownAudio

-- | An upperbound on the time it will take for audio to shutdown gracefully (see 'stopAudioGracefully').
maxShutdownDurationMicros :: Int
maxShutdownDurationMicros = 1000*12
