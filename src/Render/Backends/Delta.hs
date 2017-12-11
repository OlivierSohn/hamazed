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
preferredBuffering = BlockBuffering $ Just (maxBound :: Int)

beginFrame :: IO ()
beginFrame = return ()

endFrame :: RenderState -> IO ()
endFrame = renderFrame True {- clear buffer after rendering -}

moveTo :: Coords -> RenderState -> RenderState
moveTo =
  setDrawLocation
