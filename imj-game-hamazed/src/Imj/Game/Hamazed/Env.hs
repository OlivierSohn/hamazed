-- Environment of the Hamazed game, also used as an example in "Imj.Graphics.Render.Delta" :
-- /from a MonadIO, MonadReader YourEnv monad/

{-# OPTIONS_HADDOCK hide #-}

module Imj.Game.Hamazed.Env
      ( Env(..)
      ) where

import           Control.Distributed.Process.Lifted(receiveChanTimeout)

import           Imj.Graphics.Class.Canvas(Canvas(..))
import           Imj.Graphics.Class.Draw(Draw(..))
import           Imj.Graphics.Class.Render(Render(..))
import           Imj.Graphics.Render.Delta(DeltaEnv)
import           Imj.Input.Types
import           Imj.Game.Hamazed.Server

-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program
data Env a = Env {
    _envDeltaEnv :: !DeltaEnv
  , _envPlayerInput :: !a
  , getServerEvtChan :: !ServerEvtChan
}


-- | Forwards to the 'Draw' instance of 'DeltaEnv'.
instance Draw (Env x) where
  setScissor     (Env a _ _) = setScissor     a
  getScissor'    (Env a _ _) = getScissor'    a
  fill'          (Env a _ _) = fill'          a
  drawChar'      (Env a _ _) = drawChar'      a
  drawChars'     (Env a _ _) = drawChars'     a
  drawTxt'       (Env a _ _) = drawTxt'       a
  drawStr'       (Env a _ _) = drawStr'       a
  changeFont'    (Env a _ _) = changeFont'       a
  {-# INLINABLE setScissor #-}
  {-# INLINABLE getScissor' #-}
  {-# INLINABLE fill' #-}
  {-# INLINE drawChar' #-}
  {-# INLINE drawChars' #-}
  {-# INLINE drawTxt' #-}
  {-# INLINE drawStr' #-}
  {-# INLINE changeFont' #-}

-- | Forwards to the 'Canvas' instance of 'DeltaEnv'.
instance Canvas (Env x) where
  getTargetSize' (Env a _ _) = getTargetSize' a
  {-# INLINE getTargetSize' #-}

-- | Forwards to the 'Render' instance of 'DeltaEnv'.
instance Render (Env x) where
  renderToScreen' (Env a _ _) = renderToScreen' a
  {-# INLINE renderToScreen' #-}

instance ClientNode (Env x) where
  send _ _ = undefined
  tryReceive (Env _ _ (ServerEvtChan _ rcvPort)) =
    receiveChanTimeout 0 rcvPort
  {-# INLINABLE send #-}
  {-# INLINABLE tryReceive #-}

instance PlayerInput x => PlayerInput (Env x) where
  getKey (Env _ a _) = getKey a
  getKeyBefore (Env _ a _) = getKeyBefore a
  tryGetKey (Env _ a _) = tryGetKey a
  someInputIsAvailable (Env _ a _) = someInputIsAvailable a
  programShouldEnd (Env _ a _) = programShouldEnd a
  {-# INLINE getKey #-}
  {-# INLINE getKeyBefore #-}
  {-# INLINE tryGetKey #-}
  {-# INLINE someInputIsAvailable #-}
  {-# INLINE programShouldEnd #-}
