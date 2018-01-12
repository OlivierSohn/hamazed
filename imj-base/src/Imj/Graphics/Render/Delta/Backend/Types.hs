
module Imj.Graphics.Render.Delta.Backend.Types
       ( Backend(..)
       , DeltaRendering(..)
       ) where

import           Imj.Graphics.Render.Delta.Types

data DeltaRendering = Console
                    -- ^ Render using ascii characters in the terminal
                    | OpenGL
                    -- ^ Render using OpenGL commands in a GLFW window.

data Backend = Backend {
    _backendRun :: !(IO ())
    -- ^ Initializes the backend.
  , _backendRender :: !(Delta -> Dim Width -> IO ())
  , _backendCleanup :: !(IO ())
  -- ^ Cleanup.
}
