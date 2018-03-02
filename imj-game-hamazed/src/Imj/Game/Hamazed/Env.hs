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
  drawGlyph'      (Env a _ _) = drawGlyph'      a
  drawGlyphs'     (Env a _ _) = drawGlyphs'     a
  drawTxt'       (Env a _ _) = drawTxt'       a
  drawStr'       (Env a _ _) = drawStr'       a
  {-# INLINABLE setScissor #-}
  {-# INLINABLE getScissor' #-}
  {-# INLINABLE fill' #-}
  {-# INLINE drawGlyph' #-}
  {-# INLINE drawGlyphs' #-}
  {-# INLINE drawTxt' #-}
  {-# INLINE drawStr' #-}

-- | Forwards to the 'Canvas' instance of 'DeltaEnv'.
instance Canvas (Env x) where
  getTargetSize' (Env a _ _) = getTargetSize' a
  {-# INLINE getTargetSize' #-}

-- | Forwards to the 'Render' instance of 'DeltaEnv'.
instance Render (Env x) where
  renderToScreen'        (Env a _ _) = renderToScreen' a
  cycleRenderingOptions' (Env a _ _) = cycleRenderingOptions' a
  {-# INLINE renderToScreen' #-}
  {-# INLINE cycleRenderingOptions' #-}

instance ClientNode (Env x) where
  sendToServer' (Env _ _ q) = sendToServer' q
  serverQueue   (Env _ _ q) = serverQueue q
  {-# INLINABLE sendToServer' #-}
  {-# INLINABLE serverQueue #-}

instance PlayerInput x => PlayerInput (Env x) where
  plaformQueue (Env _ a _) = plaformQueue a
  programShouldEnd (Env _ a _) = programShouldEnd a
  pollKeys (Env _ a _) = pollKeys a
  queueType (Env _ a _) = queueType a
  waitKeysTimeout (Env _ a _) = waitKeysTimeout a
  {-# INLINE plaformQueue #-}
  {-# INLINE programShouldEnd #-}
  {-# INLINE pollKeys #-}
  {-# INLINE queueType #-}
  {-# INLINE waitKeysTimeout #-}
