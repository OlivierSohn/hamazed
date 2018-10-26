-- Environment of the Hamazed game, also used as an example in "Imj.Graphics.Render.Delta" :
-- /from a MonadIO, MonadReader YourEnv monad/

{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Env
      ( Env(..)
      , mkEnv
      ) where

import qualified Control.Concurrent.MVar as Lazy(newMVar)
import qualified Data.IntMap as Map(empty)

import           Imj.Game.Class
import           Imj.Graphics.Class.Canvas(Canvas(..))
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Class.Render(Render(..))
import           Imj.Input.Types

import           Imj.Game.Audio.Class
import           Imj.Game.Network.ClientQueues
import           Imj.Graphics.Render.Delta(DeltaEnv)
import           Imj.Control.Concurrent.AsyncGroups.Impl

-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program
data Env i g = Env {
    _envDeltaEnv :: !DeltaEnv
  , _envPlayerInput :: !i
  , _envClientQueues :: !(ClientQueues g)
  , asyncGroups :: !AsyncGroupsImpl
  , getSizedFace' :: !SizedFace
  , audio :: !(AudioT g)
  -- ^ Font to draw 'RasterizedString's
}
instance HasSizedFace (Env i g) where
  getSizedFace = getSizedFace'

mkEnv :: DeltaEnv -> i -> ClientQueues g -> SizedFace -> AudioT g -> IO (Env i g)
mkEnv a b c d e = do
  m <- AsyncGroupsImpl <$> Lazy.newMVar Map.empty
  return $ Env a b c m d e

instance Audio (AudioT g) => Audio (Env i g) where
  defaultAudio = error "logic"
  withAudio
    (Env _ _ _ _ _ a) = withAudio a
  triggerLaserSound
    (Env _ _ _ _ _ a) = triggerLaserSound a
  playMusic
    (Env _ _ _ _ _ a) = playMusic a
  {-# INLINE defaultAudio #-}
  {-# INLINE triggerLaserSound #-}
  {-# INLINE playMusic #-}
  {-# INLINE withAudio #-}

-- | Forwards to the 'Draw' instance of 'DeltaEnv'.
instance Draw (Env i g) where
  setScissor     (Env a _ _ _ _ _) = setScissor     a
  getScissor'    (Env a _ _ _ _ _) = getScissor'    a
  fill'          (Env a _ _ _ _ _) = fill'          a
  drawGlyph'     (Env a _ _ _ _ _) = drawGlyph'      a
  drawGlyphs'    (Env a _ _ _ _ _) = drawGlyphs'     a
  drawTxt'       (Env a _ _ _ _ _) = drawTxt'       a
  drawStr'       (Env a _ _ _ _ _) = drawStr'       a
  {-# INLINE setScissor #-}
  {-# INLINE getScissor' #-}
  {-# INLINE fill' #-}
  {-# INLINE drawGlyph' #-}
  {-# INLINE drawGlyphs' #-}
  {-# INLINE drawTxt' #-}
  {-# INLINE drawStr' #-}

-- | Forwards to the 'Canvas' instance of 'DeltaEnv'.
instance Canvas (Env i g) where
  getTargetSize'   (Env a _ _ _ _ _) = getTargetSize' a
  onTargetChanged' (Env a _ _ _ _ _) = onTargetChanged' a
  {-# INLINE getTargetSize' #-}
  {-# INLINE onTargetChanged' #-}

-- | Forwards to the 'Render' instance of 'DeltaEnv'.
instance Render (Env i g) where
  renderToScreen'        (Env a _ _ _ _ _) = renderToScreen' a
  cycleRenderingOptions' (Env a _ _ _ _ _) = cycleRenderingOptions' a
  applyFontMarginDelta   (Env a _ _ _ _ _) = applyFontMarginDelta a
  applyPPUDelta          (Env a _ _ _ _ _) = applyPPUDelta a
  {-# INLINE renderToScreen' #-}
  {-# INLINE cycleRenderingOptions' #-}

instance (GameLogic g) => Client (Env i g) where
  type GameLogicT (Env i g) = g
  sendToServer'  (Env _ _ q _ _ _) = sendToServer' q
  writeToClient' (Env _ _ q _ _ _) = writeToClient' q
  serverQueue    (Env _ _ q _ _ _) = serverQueue q
  {-# INLINE sendToServer' #-}
  {-# INLINE writeToClient' #-}
  {-# INLINE serverQueue #-}

instance AsyncGroups (Env i g) where
  belongsTo' (Env _ _ _ g _ _) = belongsTo' g
  cancel' (Env _ _ _ g _ _) = cancel' g
  {-# INLINABLE belongsTo' #-}
  {-# INLINABLE cancel' #-}

instance PlayerInput i => PlayerInput (Env i g) where
  plaformQueue     (Env _ a _ _ _ _) = plaformQueue a
  programShouldEnd (Env _ a _ _ _ _) = programShouldEnd a
  pollKeys         (Env _ a _ _ _ _) = pollKeys a
  waitKeys         (Env _ a _ _ _ _) = waitKeys a
  stopWaitKeys     (Env _ a _ _ _ _) = stopWaitKeys a
  queueType        (Env _ a _ _ _ _) = queueType a
  waitKeysTimeout  (Env _ a _ _ _ _) = waitKeysTimeout a
  {-# INLINE plaformQueue #-}
  {-# INLINE programShouldEnd #-}
  {-# INLINE pollKeys #-}
  {-# INLINE waitKeys #-}
  {-# INLINE stopWaitKeys #-}
  {-# INLINE queueType #-}
  {-# INLINE waitKeysTimeout #-}
