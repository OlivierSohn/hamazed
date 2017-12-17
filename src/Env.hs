
module Env(
         Env
       , createEnv
       ) where

import           Draw.Class
import           Render.Delta


-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program
newtype Env = Env {
    _envRenderBuffers :: DeltaEnv
}

-- | Forwards to the 'Draw' instance of 'DeltaEnv'.
instance Draw Env where
  drawChar_      (Env a) = drawChar_      a
  drawChars_     (Env a) = drawChars_     a
  drawTxt_       (Env a) = drawTxt_       a
  renderDrawing_ (Env a) = renderDrawing_ a
  {-# INLINE drawChar_ #-}
  {-# INLINE drawChars_ #-}
  {-# INLINE drawTxt_ #-}
  {-# INLINE renderDrawing_ #-}

-- | Contructor of 'Env'
createEnv :: IO Env
createEnv = Env . DeltaEnv <$> newDefaultContext
