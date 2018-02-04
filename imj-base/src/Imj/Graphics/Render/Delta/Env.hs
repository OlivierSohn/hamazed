{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

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
import           Data.IORef( IORef, readIORef, writeIORef )
import           Data.Maybe( fromMaybe )

import           Imj.Graphics.Class.Canvas
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Render
import           Imj.Graphics.Render.Delta.Buffers
import           Imj.Graphics.Render.Delta.DefaultPolicies
import           Imj.Graphics.Render.Delta.Draw
import           Imj.Graphics.Render.Delta.Flush
import           Imj.Graphics.Render.Delta.Types

data DeltaEnv = DeltaEnv {
    _deltaEnvBuffers :: !(IORef Buffers)
  , _deltaEnvRenderFunction :: !(Delta -> Dim Width -> IO (Time Duration System, Time Duration System))
  , _deltaEnvTargetSize :: !(IO (Maybe Size))
  , _deltaEnvCycleRenderingOptions :: !(IO ())
}

-- | Draws using the delta rendering engine.
instance Draw DeltaEnv where
  fill'          (DeltaEnv a _ _ _) b c     = liftIO $ deltaFill a b c
  setScissor     (DeltaEnv a _ _ _) b       = liftIO $ deltaSetScissor a b
  getScissor'    (DeltaEnv a _ _ _)         = liftIO $ deltaGetScissor a
  drawChar'      (DeltaEnv a _ _ _) b c d   = liftIO $ deltaDrawChar  a b c d
  drawChars'     (DeltaEnv a _ _ _) b c d e = liftIO $ deltaDrawChars a b c d e
  drawTxt'       (DeltaEnv a _ _ _) b c d   = liftIO $ deltaDrawTxt   a b c d
  drawStr'       (DeltaEnv a _ _ _) b c d   = liftIO $ deltaDrawStr   a b c d
  changeFont'    (DeltaEnv a _ _ f)         = liftIO $ f >> deltaForgetFrontValues a
  {-# INLINABLE fill' #-}
  {-# INLINABLE setScissor #-}
  {-# INLINABLE getScissor' #-}
  {-# INLINABLE drawChar' #-}
  {-# INLINABLE drawChars' #-}
  {-# INLINABLE drawTxt' #-}
  {-# INLINABLE drawStr' #-}
  {-# INLINABLE changeFont' #-}

instance Canvas DeltaEnv where
  getTargetSize' (DeltaEnv _ _ s _)         = liftIO s
  {-# INLINABLE getTargetSize' #-}

-- | Renders using the delta rendering engine.
instance Render DeltaEnv where
  renderToScreen' (DeltaEnv a b c _)         = liftIO $ deltaFlush     a b c
  {-# INLINABLE renderToScreen' #-}


class DeltaRenderBackend a where
    -- | returns (duration to issue commands, duration to flush)
    render :: (MonadIO m) => a -> Delta -> Dim Width -> m (Time Duration System, Time Duration System)
    cleanup :: (MonadIO m) => a -> m ()
    getDiscreteSize :: (MonadIO m) => a -> m (Maybe Size)
    cycleRenderingOption :: (MonadIO m) => a -> m ()

    withPolicies :: (MonadIO m)
                 => Maybe ResizePolicy
                 -> Maybe ClearPolicy
                 -> Maybe (Color8 Background)
                 -- ^ Color to clear with
                 -> (DeltaEnv -> m ())
                 -> a
                 -> m ()
    withPolicies p1 p2 p3 action ctxt = do
      newEnv ctxt p1 p2 p3 >>= action
      cleanup ctxt -- TODO use http://zguide.zeromq.org/hs:interrupt to make sure this will be called

    withDefaultPolicies :: (MonadIO m) => (DeltaEnv -> m ()) -> a -> m ()
    withDefaultPolicies =
      withPolicies Nothing Nothing Nothing


-- | Creates an environment with policies.
newEnv :: (MonadIO m, DeltaRenderBackend a)
       => a
       -> Maybe ResizePolicy
       -> Maybe ClearPolicy
       -> Maybe (Color8 Background)
       -> m DeltaEnv
newEnv backend a b c = do
  ctxt <- liftIO $ newContext a b c (getDiscreteSize backend)
  return $ DeltaEnv ctxt (render backend) (getDiscreteSize backend) (cycleRenderingOption backend)


-- | Sets the 'ResizePolicy' for back and front buffers.
-- Defaults to 'defaultResizePolicy' when Nothing is passed.
setResizePolicy :: Maybe ResizePolicy
                -> DeltaEnv
                -> IO ()
setResizePolicy mayResizePolicy (DeltaEnv ref _ _ _) =
  readIORef ref
    >>= \(Buffers a b c d e (Policies _ f g)) -> do
      let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
      writeIORef ref $ Buffers a b c d e (Policies resizePolicy f g)


-- | Sets the 'ClearPolicy'.
-- | Defaults to 'defaultClearPolicy' when Nothing is passed.
setClearPolicy :: Maybe ClearPolicy
               -> DeltaEnv
               -> IO ()
setClearPolicy mayClearPolicy (DeltaEnv ref _ _ _) =
  readIORef ref
    >>= \(Buffers a b c d e (Policies f _ clearColor)) -> do
      let clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
          buffers = Buffers a b c d e (Policies f clearPolicy clearColor)
      writeIORef ref buffers

-- | Sets the 'Color8' to use when clearing.
--   Defaults to 'defaultClearColor' when Nothing is passed.
setClearColor :: Maybe (Color8 Background)
              -> DeltaEnv
              -> IO ()
setClearColor mayClearColor (DeltaEnv ref _ _ _) =
  readIORef ref
    >>= \(Buffers a b c d e (Policies f clearPolicy _)) -> do
      let clearColor = fromMaybe defaultClearColor mayClearColor
          buffers = Buffers a b c d e (Policies f clearPolicy clearColor)
      writeIORef ref buffers


deltaSetScissor :: IORef Buffers
                -> Scissor
                -> IO ()
deltaSetScissor ref v =
  readIORef ref
    >>= \(Buffers a b c _ e f) ->
          writeIORef ref (Buffers a b c v e f)

deltaGetScissor :: IORef Buffers
                -> IO Scissor
deltaGetScissor ref =
  readIORef ref
    >>= \(Buffers _ _ _ viewport _ _) ->
          return viewport
