-- Environment of the Hamazed game, also used as an example in "Imj.Graphics.Render.Delta" :
-- /from a MonadIO, MonadReader YourEnv monad/

{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Hamazed.Env
      ( Env(..)
      , mkEnv
      ) where

import qualified Control.Concurrent.MVar as Lazy(newMVar)
import qualified Data.Map as Map(empty)

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Types(WorldId)
import           Imj.Graphics.Class.Canvas(Canvas(..))
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Class.Render(Render(..))
import           Imj.Input.Types

import           Imj.Graphics.Render.Delta(DeltaEnv)
import           Imj.Client.Class
import           Imj.Client
import           Imj.Game.Hamazed.Network.Class.AsyncGroups

-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program
data Env i = Env {
    _envDeltaEnv :: !DeltaEnv
  , _envPlayerInput :: !i
  , _envClientQueues :: !(ClientQueues Event Hamazed)
  , asyncGroups :: !(RequestsAsyncs WorldId)
  , getSizedFace' :: !SizedFace
  -- ^ Font to draw 'RasterizedString's
}
instance HasSizedFace (Env x) where
  getSizedFace = getSizedFace'

mkEnv :: DeltaEnv -> i -> ClientQueues Event Hamazed -> SizedFace -> IO (Env i)
mkEnv a b c d = do
  m <- RequestsAsyncs <$> Lazy.newMVar Map.empty
  return $ Env a b c m d


-- | Forwards to the 'Draw' instance of 'DeltaEnv'.
instance Draw (Env i) where
  setScissor     (Env a _ _ _ _) = setScissor     a
  getScissor'    (Env a _ _ _ _) = getScissor'    a
  fill'          (Env a _ _ _ _) = fill'          a
  drawGlyph'     (Env a _ _ _ _) = drawGlyph'      a
  drawGlyphs'    (Env a _ _ _ _) = drawGlyphs'     a
  drawTxt'       (Env a _ _ _ _) = drawTxt'       a
  drawStr'       (Env a _ _ _ _) = drawStr'       a
  {-# INLINABLE setScissor #-}
  {-# INLINABLE getScissor' #-}
  {-# INLINABLE fill' #-}
  {-# INLINE drawGlyph' #-}
  {-# INLINE drawGlyphs' #-}
  {-# INLINE drawTxt' #-}
  {-# INLINE drawStr' #-}

-- | Forwards to the 'Canvas' instance of 'DeltaEnv'.
instance Canvas (Env i) where
  getTargetSize'   (Env a _ _ _ _) = getTargetSize' a
  onTargetChanged' (Env a _ _ _ _) = onTargetChanged' a
  {-# INLINE getTargetSize' #-}
  {-# INLINE onTargetChanged' #-}

-- | Forwards to the 'Render' instance of 'DeltaEnv'.
instance Render (Env i) where
  renderToScreen'        (Env a _ _ _ _) = renderToScreen' a
  cycleRenderingOptions' (Env a _ _ _ _) = cycleRenderingOptions' a
  applyFontMarginDelta   (Env a _ _ _ _) = applyFontMarginDelta a
  applyPPUDelta          (Env a _ _ _ _) = applyPPUDelta a
  {-# INLINE renderToScreen' #-}
  {-# INLINE cycleRenderingOptions' #-}

instance Client (Env i) where
  type ServerT (Env i) = Hamazed
  type CliEvtT (Env i) = Event
  sendToServer'  (Env _ _ q _ _) = sendToServer' q
  writeToClient' (Env _ _ q _ _) = writeToClient' q
  serverQueue    (Env _ _ q _ _) = serverQueue q
  {-# INLINABLE sendToServer' #-}
  {-# INLINABLE writeToClient' #-}
  {-# INLINABLE serverQueue #-}

instance AsyncGroups (Env i) where
  type Key (Env i) = WorldId
  belongsTo' (Env _ _ _ g _) = belongsTo' g
  cancel' (Env _ _ _ g _) = cancel' g
  {-# INLINABLE belongsTo' #-}
  {-# INLINABLE cancel' #-}

instance PlayerInput i => PlayerInput (Env i) where
  plaformQueue     (Env _ a _ _ _) = plaformQueue a
  programShouldEnd (Env _ a _ _ _) = programShouldEnd a
  pollKeys         (Env _ a _ _ _) = pollKeys a
  queueType        (Env _ a _ _ _) = queueType a
  waitKeysTimeout  (Env _ a _ _ _) = waitKeysTimeout a
  {-# INLINE plaformQueue #-}
  {-# INLINE programShouldEnd #-}
  {-# INLINE pollKeys #-}
  {-# INLINE queueType #-}
  {-# INLINE waitKeysTimeout #-}
