-- Environment of the Hamazed game, also used as an example in "Imj.Graphics.Render.Delta" :
-- /from a MonadIO, MonadReader YourEnv monad/

{-# OPTIONS_HADDOCK hide #-}

module Imj.Game.Hamazed.Env
      ( Env(..)
      ) where

import           Imj.Graphics.Class.Draw(Draw(..))
import           Imj.Graphics.Class.Render(Render(..))
import           Imj.Graphics.Render.Delta(DeltaEnv)


-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program
newtype Env = Env {
    _envDeltaEnv :: DeltaEnv
}

-- | Forwards to the 'Draw' instance of 'DeltaEnv'.
instance Draw Env where
  setScissor     (Env a) = setScissor     a
  getScissor'    (Env a) = getScissor'    a
  fill'          (Env a) = fill'          a
  drawChar'      (Env a) = drawChar'      a
  drawChars'     (Env a) = drawChars'     a
  drawTxt'       (Env a) = drawTxt'       a
  drawStr'       (Env a) = drawStr'       a
  getTargetSize' (Env a) = getTargetSize' a
  {-# INLINABLE setScissor #-}
  {-# INLINABLE getScissor' #-}
  {-# INLINABLE fill' #-}
  {-# INLINE drawChar' #-}
  {-# INLINE drawChars' #-}
  {-# INLINE drawTxt' #-}
  {-# INLINE drawStr' #-}
  {-# INLINE getTargetSize' #-}
-- | Forwards to the 'Render' instance of 'DeltaEnv'.
instance Render Env where
  renderToScreen' (Env a) = renderToScreen' a
  {-# INLINE renderToScreen' #-}
