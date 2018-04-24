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

foreign import ccall "beep" beep :: IO ()
foreign import ccall "initializeAudio" initializeAudio :: IO ()
foreign import ccall "finalizeAudio" teardownAudio :: IO ()
foreign import ccall "midiNoteOn" midiNoteOn :: CInt -> CFloat -> IO ()
foreign import ccall "midiNoteOff" midiNoteOff :: CInt -> IO ()

withAudio :: IO () -> IO ()
withAudio act = do
  initializeAudio
  act
  teardownAudio
