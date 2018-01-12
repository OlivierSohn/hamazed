{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Graphics.Render.Delta.Env
    ( DeltaEnv
    , newDefaultEnv
    , newEnv
    , ResizePolicy(..)
    , defaultResizePolicy
    , setResizePolicy
    , ClearPolicy(..)
    , defaultClearPolicy
    , setClearPolicy
    , defaultClearColor
    , setClearColor
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(liftIO)
import           Data.IORef( IORef, readIORef, writeIORef )
import           Data.Maybe( fromMaybe )

import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Render
import           Imj.Graphics.Render.Delta.Buffers
import           Imj.Graphics.Render.Delta.DefaultPolicies
import           Imj.Graphics.Render.Delta.Draw
import           Imj.Graphics.Render.Delta.Flush
import           Imj.Graphics.Render.Delta.Types

data DeltaEnv = DeltaEnv {
    _deltaEnvBuffers :: !(IORef Buffers)
  , _deltaEnvRenderFunction :: !(Delta -> Dim Width -> IO ())
}

-- | Draws using the delta rendering engine.
instance Draw DeltaEnv where
  fill'          (DeltaEnv a _) b c     = liftIO $ deltaFill a b c
  setScissor     (DeltaEnv a _) b       = liftIO $ deltaSetScissor a b
  getScissor'    (DeltaEnv a _)         = liftIO $ deltaGetScissor a
  drawChar'      (DeltaEnv a _) b c d   = liftIO $ deltaDrawChar  a b c d
  drawChars'     (DeltaEnv a _) b c d e = liftIO $ deltaDrawChars a b c d e
  drawTxt'       (DeltaEnv a _) b c d   = liftIO $ deltaDrawTxt   a b c d
  drawStr'       (DeltaEnv a _) b c d   = liftIO $ deltaDrawStr   a b c d
  {-# INLINABLE fill' #-}
  {-# INLINABLE setScissor #-}
  {-# INLINABLE getScissor' #-}
  {-# INLINABLE drawChar' #-}
  {-# INLINABLE drawChars' #-}
  {-# INLINABLE drawTxt' #-}
  {-# INLINABLE drawStr' #-}
-- | Renders using the delta rendering engine.
instance Render DeltaEnv where
  renderToScreen' (DeltaEnv a b)         = liftIO $ deltaFlush     a b
  {-# INLINABLE renderToScreen' #-}


-- | Creates an environment using default policies.
newDefaultEnv :: (Delta -> Dim Width -> IO ())
              -> IO DeltaEnv
newDefaultEnv renderFunc =
  newEnv renderFunc Nothing Nothing Nothing

-- | Creates an environment with policies.
newEnv :: (Delta -> Dim Width -> IO ())
       -> Maybe ResizePolicy
       -> Maybe ClearPolicy
       -> Maybe (Color8 Background)
       -> IO DeltaEnv
newEnv renderFunc a b c =
  (`DeltaEnv` renderFunc) <$> newContext a b c


-- | Sets the 'ResizePolicy' for back and front buffers.
-- Defaults to 'defaultResizePolicy' when Nothing is passed.
setResizePolicy :: Maybe ResizePolicy
                -> DeltaEnv
                -> IO ()
setResizePolicy mayResizePolicy (DeltaEnv ref _) =
  readIORef ref
    >>= \(Buffers a b c d e (Policies _ f g)) -> do
      let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
      writeIORef ref $ Buffers a b c d e (Policies resizePolicy f g)


-- | Sets the 'ClearPolicy'.
-- | Defaults to 'defaultClearPolicy' when Nothing is passed.
setClearPolicy :: Maybe ClearPolicy
               -> DeltaEnv
               -> IO ()
setClearPolicy mayClearPolicy (DeltaEnv ref _) =
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
setClearColor mayClearColor (DeltaEnv ref _) =
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
