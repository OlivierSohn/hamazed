{-# LANGUAGE NoImplicitPrelude #-}

module Render.Backends.Delta
                          ( beginFrame
                          , endFrame
                          , moveTo
                          , preferredBuffering
                          -- reexports
                          , module Render.Backends.Internal.Delta
                          ) where

import           Imajuscule.Prelude

import           System.IO( BufferMode(..) )

import           Geo.Discrete.Types

import           Render.Backends.Internal.Delta

preferredBuffering :: BufferMode
preferredBuffering = BlockBuffering Nothing

beginFrame :: IO ()
beginFrame = return ()

endFrame :: IO ()
endFrame = submitDrawing True {- clear buffer after rendering -}

moveTo :: Coords -> IO ()
moveTo (Coords (Row r) (Col c)) =
  setDrawLocation c r
