{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Render.Delta.Env
    ( DeltaEnv
    , newEnv
    , ResizePolicy(..)
    , defaultResizePolicy
    , setResizePolicy
    , ClearPolicy(..)
    , defaultClearPolicy
    , setClearPolicy
    , defaultClearColor
    , setClearColor
    , DeltaRenderBackend(..)
    , Size(..)
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.IO.Unlift(MonadUnliftIO)
import           Data.IORef(IORef, readIORef, writeIORef, modifyIORef')
import           Data.Maybe(fromMaybe)
import           UnliftIO.Exception(finally)

import           Imj.Graphics.Class.Canvas
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Render
import           Imj.Graphics.Font
import           Imj.Graphics.Render.Delta.Buffers
import           Imj.Graphics.Render.Delta.DefaultPolicies
import           Imj.Graphics.Render.Delta.Draw
import           Imj.Graphics.Render.Delta.Flush
import           Imj.Graphics.Render.Delta.Types
import           Imj.Timing

data DeltaEnv = DeltaEnv {
    _deltaEnvBuffers :: !(IORef Buffers)
  , _deltaEnvRenderFunction :: !(Delta -> Dim Width -> IO (Time Duration System, Time Duration System))
  , _deltaEnvTargetSize :: !(IO (Maybe Size))
  , _deltaEnvCycleRenderingOptions :: !(CycleFont -> CycleFontSize -> IO (Either String ()))
  , _deltaEnvApplyPPUDelta :: !(PPU -> IO (Either String ()))
  , _deltaEnvApplyFontMarginDelta :: !(FontMargin -> IO (Either String ()))
}

-- | Draws using the delta rendering engine.
instance Draw DeltaEnv where
  fill'          (DeltaEnv a _ _ _ _ _) b c     = liftIO $ deltaFill a b c
  setScissor     (DeltaEnv a _ _ _ _ _) b       = liftIO $ deltaSetScissor a b
  getScissor'    (DeltaEnv a _ _ _ _ _)         = liftIO $ deltaGetScissor a
  drawGlyph'     (DeltaEnv a _ _ _ _ _) b c d   = liftIO $ deltaDrawChar  a b c d
  drawGlyphs'    (DeltaEnv a _ _ _ _ _) b c d e = liftIO $ deltaDrawChars a b c d e
  drawStr'       (DeltaEnv a _ _ _ _ _) b c d   = liftIO $ deltaDrawStr   a b c d
  {-# INLINABLE fill' #-}
  {-# INLINABLE setScissor #-}
  {-# INLINABLE getScissor' #-}
  {-# INLINABLE drawGlyph' #-}
  {-# INLINABLE drawGlyphs' #-}
  {-# INLINABLE drawStr' #-}

instance Canvas DeltaEnv where
  getTargetSize' (DeltaEnv _ _ s _ _ _) =
    liftIO s
  onTargetChanged' e =
    liftIO $ canModifyLayout e (return ())
  {-# INLINABLE getTargetSize' #-}

-- | Renders using the delta rendering engine.
instance Render DeltaEnv where
  renderToScreen' (DeltaEnv a b c _ _ _) =
    liftIO $ deltaFlush a b c
  cycleRenderingOptions' e@(DeltaEnv _ _ _ f _ _) i j =
    liftIO $ canModifyLayout e $ void $ f i j
  applyPPUDelta e@(DeltaEnv _ _ _ _ f _) d =
    liftIO $ canModifyLayout e $ void $ f d
  applyFontMarginDelta e@(DeltaEnv _ _ _ _ _ f) marginDelta =
    liftIO $ canModifyLayout e $ void $ f marginDelta
  {-# INLINABLE renderToScreen' #-}
  {-# INLINABLE cycleRenderingOptions' #-}

canModifyLayout :: DeltaEnv -> IO () -> IO (Either String ())
canModifyLayout (DeltaEnv b _ sz _ _ _) act = do
  res <- sz >>= \oldSz -> act >> sz >>= maybe
    (return $ Right ())
    (\newSz ->
      if oldSz /= Just newSz
        then
          resize newSz b
        else
          return $ Right ())
  deltaForgetFrontValues b -- even if the size didn't change, maybe the layout changed
  return res

resize :: Size -> IORef Buffers -> IO (Either String ())
resize (Size h w) b = readIORef b >>= \(Buffers _ _ _ _ _ policies) ->
  createBuffers policies (fromIntegral w) (fromIntegral h) >>= either
    (return . Left)
    (fmap Right . writeIORef b)

class DeltaRenderBackend a where
    render :: (MonadIO m) => a -> Delta -> Dim Width
           -> m (Time Duration System, Time Duration System)
           -- ^ durations to (issue commands, flush)
    cleanup :: (MonadIO m) => a -> m ()
    getDiscreteSize :: (MonadIO m) => a -> m (Maybe Size)
    cycleRenderingOption :: (MonadIO m) => a -> CycleFont -> CycleFontSize -> m (Either String ())
    ppuDelta :: (MonadIO m) => a -> PPU -> m (Either String ())
    fontMarginDelta :: (MonadIO m) => a -> FontMargin -> m (Either String ())

    withPolicies :: (MonadUnliftIO m)
                 => Maybe ResizePolicy
                 -> Maybe ClearPolicy
                 -> Maybe (Color8 Background)
                 -- ^ Color to clear with
                 -> (DeltaEnv -> m ())
                 -> a
                 -> m (Either String ())
    withPolicies p1 p2 p3 action ctxt =
      flip finally (cleanup ctxt)
        $ newEnv ctxt p1 p2 p3 >>= either (return . Left) (fmap Right . action)

    withDefaultPolicies :: (MonadUnliftIO m) => (DeltaEnv -> m ()) -> a -> m ()
    withDefaultPolicies a b = withPolicies Nothing Nothing Nothing a b >>= either error return


-- | Creates an environment with policies.
newEnv :: (MonadIO m, DeltaRenderBackend a)
       => a
       -> Maybe ResizePolicy
       -> Maybe ClearPolicy
       -> Maybe (Color8 Background)
       -> m (Either String DeltaEnv)
newEnv backend a b c = do
  let sz = getDiscreteSize backend
  liftIO $ newContext a b c sz >>= either
    (return . Left)
    (\ctxt -> return $ Right $ DeltaEnv
      ctxt
      (render backend)
      sz
      (cycleRenderingOption backend)
      (ppuDelta backend)
      (fontMarginDelta backend))


-- | Sets the 'ResizePolicy' for back and front buffers.
-- Defaults to 'defaultResizePolicy' when Nothing is passed.
setResizePolicy :: Maybe ResizePolicy
                -> DeltaEnv
                -> IO ()
setResizePolicy mayResizePolicy (DeltaEnv ref _ _ _ _ _) =
  modifyIORef' ref $ \b@(Buffers _ _ _ _ _ (Policies _ f g)) ->
    let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
    in b { getPolicies = Policies resizePolicy f g }


-- | Sets the 'ClearPolicy'.
-- | Defaults to 'defaultClearPolicy' when Nothing is passed.
setClearPolicy :: Maybe ClearPolicy
               -> DeltaEnv
               -> IO ()
setClearPolicy mayClearPolicy (DeltaEnv ref _ _ _ _ _) =
  modifyIORef' ref $ \b@(Buffers _ _ _ _ _ (Policies f _ clearColor)) ->
    let clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
    in b { getPolicies = Policies f clearPolicy clearColor }

-- | Sets the 'Color8' to use when clearing.
--   Defaults to 'defaultClearColor' when Nothing is passed.
setClearColor :: Maybe (Color8 Background)
              -> DeltaEnv
              -> IO ()
setClearColor mayClearColor (DeltaEnv ref _ _ _ _ _) =
  modifyIORef' ref $ \(Buffers a b c d e (Policies f clearPolicy _)) ->
    let clearColor = fromMaybe defaultClearColor mayClearColor
    in Buffers a b c d e (Policies f clearPolicy clearColor)


deltaSetScissor :: IORef Buffers
                -> Scissor
                -> IO ()
deltaSetScissor ref v =
  modifyIORef' ref $ \(Buffers a b c _ e f) -> Buffers a b c v e f

deltaGetScissor :: IORef Buffers
                -> IO Scissor
deltaGetScissor ref =
  readIORef ref
    >>= \(Buffers _ _ _ viewport _ _) ->
          return viewport
