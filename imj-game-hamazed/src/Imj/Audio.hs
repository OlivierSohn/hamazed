{-# LANGUAGE ForeignFunctionInterface #-}

module Imj.Audio
      ( beep
      , initializeAudio
      , teardownAudio
      ) where

foreign import ccall "beep" c_beep :: IO ()
foreign import ccall "initializeAudio" c_initAudio :: IO ()
foreign import ccall "finalizeAudio" c_teardownAudio :: IO ()

initializeAudio :: IO ()
initializeAudio = c_initAudio

teardownAudio :: IO ()
teardownAudio = c_teardownAudio

beep :: IO ()
beep =
  c_beep
