-- Environment of the Hamazed game, also used as an example in "Imj.Graphics.Render.Delta" :
-- /from a MonadIO, MonadReader YourEnv monad/

{-# OPTIONS_HADDOCK hide #-}

module Imj.Game.Hamazed.Env
      ( Env(..)
      ) where

import           Imj.Graphics.Class.Draw(Draw(..))
import           Imj.Graphics.Class.Render(Render(..))
import           Imj.Graphics.Render.Delta(DeltaEnv)
import           Imj.Input.Types


-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program
data Env a = Env {
    _envDeltaEnv :: !DeltaEnv
  , _envPlayerInput :: !a
}

-- | Forwards to the 'Draw' instance of 'DeltaEnv'.
instance Draw (Env x) where
  setScissor     (Env a _) = setScissor     a
  getScissor'    (Env a _) = getScissor'    a
  fill'          (Env a _) = fill'          a
  drawChar'      (Env a _) = drawChar'      a
  drawChars'     (Env a _) = drawChars'     a
  drawTxt'       (Env a _) = drawTxt'       a
  drawStr'       (Env a _) = drawStr'       a
  getTargetSize' (Env a _) = getTargetSize' a
  {-# INLINABLE setScissor #-}
  {-# INLINABLE getScissor' #-}
  {-# INLINABLE fill' #-}
  {-# INLINE drawChar' #-}
  {-# INLINE drawChars' #-}
  {-# INLINE drawTxt' #-}
  {-# INLINE drawStr' #-}
  {-# INLINE getTargetSize' #-}
-- | Forwards to the 'Render' instance of 'DeltaEnv'.
instance Render (Env x) where
  renderToScreen' (Env a _) = renderToScreen' a
  {-# INLINE renderToScreen' #-}

instance PlayerInput x => PlayerInput (Env x) where
  getKey (Env _ a) = getKey a
  getKeyTimeout (Env _ a) = getKeyTimeout a
  tryGetKey (Env _ a) = tryGetKey a
  {-# INLINE getKey #-}
  {-# INLINE getKeyTimeout #-}
  {-# INLINE tryGetKey #-}
