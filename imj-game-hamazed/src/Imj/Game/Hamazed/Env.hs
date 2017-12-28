{-# OPTIONS_HADDOCK hide #-}

module Imj.Game.Hamazed.Env(
         Env
       , createEnv
       ) where

import           Imj.Graphics.Draw.Class(Draw(..))
import           Imj.Graphics.Render.Delta(newDefaultEnv, DeltaEnv)


-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program
newtype Env = Env {
    _envDeltaEnv :: DeltaEnv
}

-- | Forwards to the 'Draw' instance of 'DeltaEnv'.
instance Draw Env where
  drawChar'      (Env a) = drawChar'      a
  drawChars'     (Env a) = drawChars'     a
  drawTxt'       (Env a) = drawTxt'       a
  drawStr'       (Env a) = drawStr'       a
  renderDrawing' (Env a) = renderDrawing' a
  {-# INLINE drawChar' #-}
  {-# INLINE drawChars' #-}
  {-# INLINE drawTxt' #-}
  {-# INLINE drawStr' #-}
  {-# INLINE renderDrawing' #-}

-- | Constructor of 'Env'
createEnv :: IO Env
createEnv = Env <$> newDefaultEnv
