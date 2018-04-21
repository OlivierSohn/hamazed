-- Environment of the Hamazed game, also used as an example in "Imj.Graphics.Render.Delta" :
-- /from a MonadIO, MonadReader YourEnv monad/

{-# OPTIONS_HADDOCK hide #-}

module Imj.Game.Hamazed.Env
      ( Env(..)
      ) where

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Network.Class.ClientNode
import           Imj.Graphics.Class.Canvas(Canvas(..))
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Class.Render(Render(..))
import           Imj.Input.Types

import           Imj.Graphics.Render.Delta(DeltaEnv)

-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program
data Env a = Env {
    _envDeltaEnv :: !DeltaEnv
  , _envPlayerInput :: !a
  , _envClientQueues :: !ClientQueues
  , getSizedFace' :: !SizedFace
  -- ^ Font to draw 'RasterizedString's
}
instance HasSizedFace (Env x) where
  getSizedFace = getSizedFace'

-- | Forwards to the 'Draw' instance of 'DeltaEnv'.
instance Draw (Env x) where
  setScissor     (Env a _ _ _) = setScissor     a
  getScissor'    (Env a _ _ _) = getScissor'    a
  fill'          (Env a _ _ _) = fill'          a
  drawGlyph'     (Env a _ _ _) = drawGlyph'      a
  drawGlyphs'    (Env a _ _ _) = drawGlyphs'     a
  drawTxt'       (Env a _ _ _) = drawTxt'       a
  drawStr'       (Env a _ _ _) = drawStr'       a
  {-# INLINABLE setScissor #-}
  {-# INLINABLE getScissor' #-}
  {-# INLINABLE fill' #-}
  {-# INLINE drawGlyph' #-}
  {-# INLINE drawGlyphs' #-}
  {-# INLINE drawTxt' #-}
  {-# INLINE drawStr' #-}

-- | Forwards to the 'Canvas' instance of 'DeltaEnv'.
instance Canvas (Env x) where
  getTargetSize'   (Env a _ _ _) = getTargetSize' a
  onTargetChanged' (Env a _ _ _) = onTargetChanged' a
  {-# INLINE getTargetSize' #-}
  {-# INLINE onTargetChanged' #-}

-- | Forwards to the 'Render' instance of 'DeltaEnv'.
instance Render (Env x) where
  renderToScreen'        (Env a _ _ _) = renderToScreen' a
  cycleRenderingOptions' (Env a _ _ _) = cycleRenderingOptions' a
  applyFontMarginDelta   (Env a _ _ _) = applyFontMarginDelta a
  applyPPUDelta          (Env a _ _ _) = applyPPUDelta a
  {-# INLINE renderToScreen' #-}
  {-# INLINE cycleRenderingOptions' #-}

instance ClientNode (Env x) where
  sendToServer'  (Env _ _ q _) = sendToServer' q
  writeToClient' (Env _ _ q _) = writeToClient' q
  serverQueue    (Env _ _ q _) = serverQueue q
  belongsTo' (Env _ _ q _) = belongsTo' q
  cancel' (Env _ _ q _) = cancel' q
  {-# INLINABLE sendToServer' #-}
  {-# INLINABLE writeToClient' #-}
  {-# INLINABLE serverQueue #-}
  {-# INLINABLE belongsTo' #-}
  {-# INLINABLE cancel' #-}

instance PlayerInput x => PlayerInput (Env x) where
  plaformQueue     (Env _ a _ _) = plaformQueue a
  programShouldEnd (Env _ a _ _) = programShouldEnd a
  pollKeys         (Env _ a _ _) = pollKeys a
  queueType        (Env _ a _ _) = queueType a
  waitKeysTimeout  (Env _ a _ _) = waitKeysTimeout a
  {-# INLINE plaformQueue #-}
  {-# INLINE programShouldEnd #-}
  {-# INLINE pollKeys #-}
  {-# INLINE queueType #-}
  {-# INLINE waitKeysTimeout #-}
