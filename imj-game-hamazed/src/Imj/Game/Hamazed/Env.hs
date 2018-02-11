-- Environment of the Hamazed game, also used as an example in "Imj.Graphics.Render.Delta" :
-- /from a MonadIO, MonadReader YourEnv monad/

{-# OPTIONS_HADDOCK hide #-}

module Imj.Game.Hamazed.Env
      ( Env(..)
      ) where

import           Imj.Game.Hamazed.Network.Class.ClientNode
import           Imj.Game.Hamazed.Network.Types
import           Imj.Graphics.Class.Canvas(Canvas(..))
import           Imj.Graphics.Class.Draw(Draw(..))
import           Imj.Graphics.Class.Render(Render(..))
import           Imj.Graphics.Render.Delta(DeltaEnv)
import           Imj.Input.Types

-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program
data Env a = Env {
    _envDeltaEnv :: !DeltaEnv
  , _envPlayerInput :: !a
  , _envClientQueues :: !ClientQueues
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
  sendToServer' (Env _ _ q) = sendToServer' q
  serverQueue (Env _ _ q) = serverQueue q
  {-# INLINABLE sendToServer' #-}
  {-# INLINABLE serverQueue #-}

instance PlayerInput x => PlayerInput (Env x) where
  keysQueue (Env _ a _) = keysQueue a
  programShouldEnd (Env _ a _) = programShouldEnd a
  pollKeys (Env _ a _) = pollKeys a
  queueType (Env _ a _) = queueType a
  waitKeysTimeout (Env _ a _) = waitKeysTimeout a
  {-# INLINE keysQueue #-}
  {-# INLINE programShouldEnd #-}
  {-# INLINE pollKeys #-}
  {-# INLINE queueType #-}
  {-# INLINE waitKeysTimeout #-}
