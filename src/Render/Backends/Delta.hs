{-# LANGUAGE NoImplicitPrelude #-}

module Render.Backends.Delta
                          ( beginFrame
                          , endFrame
                          , preferredBuffering
                          -- reexports
                          , module Render.Backends.Internal.Delta
                          , BufferMode(..)
                          ) where

import           Imajuscule.Prelude

import           System.IO( BufferMode(..) )

import           Geo.Discrete.Types

import           Render.Backends.Internal.Delta

preferredBuffering :: BufferMode
preferredBuffering = BlockBuffering $ Just (maxBound :: Int)

beginFrame :: IO ()
beginFrame = return ()

endFrame :: IORef Buffers -> IO ()
endFrame =
  renderFrame True {- clear buffer after rendering -}
