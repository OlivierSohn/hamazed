-- Environment of the Hamazed game, also used as an example in "Imj.Graphics.Render.Delta" :
-- /from a MonadIO, MonadReader YourEnv monad/

{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Env
      ( Env(..)
      , mkEnv
      ) where

import qualified Control.Concurrent.MVar as Lazy(newMVar)
import           Control.Monad.IO.Class(liftIO)
import qualified Data.IntMap as Map(empty)

import           Imj.Game.Types
import           Imj.Graphics.Class.Canvas(Canvas(..))
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Class.Render(Render(..))
import           Imj.Input.Types

import           Imj.Music
import           Imj.Game.Audio.Class
import           Imj.Game.Configuration
import           Imj.Game.Sound
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
  , audio :: !WithAudio
  -- ^ Font to draw 'RasterizedString's
}
instance HasSizedFace (Env i g) where
  getSizedFace = getSizedFace'

mkEnv :: DeltaEnv -> i -> ClientQueues g -> SizedFace -> WithAudio -> IO (Env i g)
mkEnv a b c d e = do
  m <- AsyncGroupsImpl <$> Lazy.newMVar Map.empty
  return $ Env a b c m d e

instance Audio (Env i g) where
  triggerLaserSound (Env _ _ _ _ _ (WithAudio a))
    | a = liftIO laserSound
    | otherwise = return ()
  playMusic (Env _ _ _ _ _ (WithAudio a)) mus instr
    | a = liftIO $ play mus instr
    | otherwise = return ()
  {-# INLINABLE triggerLaserSound #-}
  {-# INLINABLE playMusic #-}

-- | Forwards to the 'Draw' instance of 'DeltaEnv'.
instance Draw (Env i g) where
  setScissor     (Env a _ _ _ _ _) = setScissor     a
  getScissor'    (Env a _ _ _ _ _) = getScissor'    a
  fill'          (Env a _ _ _ _ _) = fill'          a
  drawGlyph'     (Env a _ _ _ _ _) = drawGlyph'      a
  drawGlyphs'    (Env a _ _ _ _ _) = drawGlyphs'     a
  drawTxt'       (Env a _ _ _ _ _) = drawTxt'       a
  drawStr'       (Env a _ _ _ _ _) = drawStr'       a
  {-# INLINABLE setScissor #-}
  {-# INLINABLE getScissor' #-}
  {-# INLINABLE fill' #-}
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
  {-# INLINABLE sendToServer' #-}
  {-# INLINABLE writeToClient' #-}
  {-# INLINABLE serverQueue #-}

instance AsyncGroups (Env i g) where
  belongsTo' (Env _ _ _ g _ _) = belongsTo' g
  cancel' (Env _ _ _ g _ _) = cancel' g
  {-# INLINABLE belongsTo' #-}
  {-# INLINABLE cancel' #-}

instance PlayerInput i => PlayerInput (Env i g) where
  plaformQueue     (Env _ a _ _ _ _) = plaformQueue a
  programShouldEnd (Env _ a _ _ _ _) = programShouldEnd a
  pollKeys         (Env _ a _ _ _ _) = pollKeys a
  queueType        (Env _ a _ _ _ _) = queueType a
  waitKeysTimeout  (Env _ a _ _ _ _) = waitKeysTimeout a
  {-# INLINE plaformQueue #-}
  {-# INLINE programShouldEnd #-}
  {-# INLINE pollKeys #-}
  {-# INLINE queueType #-}
  {-# INLINE waitKeysTimeout #-}
