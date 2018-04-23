{-# LANGUAGE ForeignFunctionInterface #-}

module Imj.Audio
      ( beep
      , initializeAudio
      , teardownAudio
      ) where

foreign import ccall "beep" beep :: IO ()
foreign import ccall "initializeAudio" initializeAudio :: IO ()
foreign import ccall "finalizeAudio" teardownAudio :: IO ()
