
module Env(
         Env
       , createEnv
       ) where

import           Draw.Class(Draw(..))
import           Render.Delta(newDefaultEnv, DeltaEnv)


-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program
newtype Env = Env {
    _envDeltaEnv :: DeltaEnv
}

-- | Forwards to the 'Draw' instance of 'DeltaEnv'.
instance Draw Env where
  drawChar'      (Env a) = drawChar'      a
  drawChars'     (Env a) = drawChars'     a
  drawTxt'       (Env a) = drawTxt'       a
  renderDrawing' (Env a) = renderDrawing' a
  {-# INLINE drawChar' #-}
  {-# INLINE drawChars' #-}
  {-# INLINE drawTxt' #-}
  {-# INLINE renderDrawing' #-}

-- | Contructor of 'Env'
createEnv :: IO Env
createEnv = Env <$> newDefaultEnv
