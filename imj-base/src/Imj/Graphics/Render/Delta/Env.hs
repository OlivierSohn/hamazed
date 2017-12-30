{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}


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
    , defaultStdoutMode
    , setStdoutBufferMode
    -- * Reexports
    , BufferMode(..)
    ) where

import           Imj.Prelude

import           System.IO(BufferMode(..), hSetBuffering, stdout)

import           Control.Monad.IO.Class(liftIO)

import           Data.IORef( IORef, readIORef, writeIORef )
import           Data.Maybe( fromMaybe )

import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Render
import           Imj.Graphics.Render.Delta.Buffers
import           Imj.Graphics.Render.Delta.Console
import           Imj.Graphics.Render.Delta.DefaultPolicies
import           Imj.Graphics.Render.Delta.Draw
import           Imj.Graphics.Render.Delta.Flush
import           Imj.Graphics.Render.Delta.Types

newtype DeltaEnv = DeltaEnv (IORef Buffers)

-- | Draws using the delta rendering engine.
instance Draw DeltaEnv where
  drawChar'      (DeltaEnv a) b c d   = liftIO $ deltaDrawChar  a b c d
  drawChars'     (DeltaEnv a) b c d e = liftIO $ deltaDrawChars a b c d e
  drawTxt'       (DeltaEnv a) b c d   = liftIO $ deltaDrawTxt   a b c d
  drawStr'       (DeltaEnv a) b c d   = liftIO $ deltaDrawStr   a b c d
  {-# INLINABLE drawChar' #-}
  {-# INLINABLE drawChars' #-}
  {-# INLINABLE drawTxt' #-}
  {-# INLINABLE drawStr' #-}
-- | Renders using the delta rendering engine.
instance Render DeltaEnv where
  renderToScreen' (DeltaEnv a)         = liftIO $ deltaFlush     a
  {-# INLINABLE renderToScreen' #-}


-- | Creates an environment using default policies.
newDefaultEnv :: IO DeltaEnv
newDefaultEnv = newEnv Nothing Nothing Nothing Nothing

-- | Creates an environment with policies.
newEnv :: Maybe ResizePolicy
       -> Maybe ClearPolicy
       -> Maybe (Color8 Background)
       -> Maybe BufferMode
       -- ^ Preferred stdout 'BufferMode'.
       -> IO DeltaEnv
newEnv a b c mayBufferMode = do
  let stdoutBufMode = fromMaybe defaultStdoutMode mayBufferMode
  configureConsoleFor Gaming stdoutBufMode
  DeltaEnv <$> newContext a b c


-- | Sets the 'ResizePolicy' for back and front buffers.
-- Defaults to 'defaultResizePolicy' when Nothing is passed.
setResizePolicy :: Maybe ResizePolicy
                -> DeltaEnv
                -> IO ()
setResizePolicy mayResizePolicy (DeltaEnv ref) =
  readIORef ref
    >>= \(Buffers a b d e (Policies _ f g)) -> do
      let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
      writeIORef ref $ Buffers a b d e (Policies resizePolicy f g)


-- | Sets the 'ClearPolicy'.
-- | Defaults to 'defaultClearPolicy' when Nothing is passed.
setClearPolicy :: Maybe ClearPolicy
               -> DeltaEnv
               -> IO ()
setClearPolicy mayClearPolicy (DeltaEnv ref) =
  readIORef ref
    >>= \(Buffers a b d e (Policies f _ clearColor)) -> do
      let clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
          buffers = Buffers a b d e (Policies f clearPolicy clearColor)
      writeIORef ref buffers

-- | Sets the 'Color8' to use when clearing.
--   Defaults to 'defaultClearColor' when Nothing is passed.
setClearColor :: Maybe (Color8 Background)
              -> DeltaEnv
              -> IO ()
setClearColor mayClearColor (DeltaEnv ref) =
  readIORef ref
    >>= \(Buffers a b d e (Policies f clearPolicy _)) -> do
      let clearColor = fromMaybe defaultClearColor mayClearColor
          buffers = Buffers a b d e (Policies f clearPolicy clearColor)
      writeIORef ref buffers

-- | Sets stdout's 'BufferMode'. Defaults to 'defaultStdoutMode' when Nothing is passed.
setStdoutBufferMode :: Maybe BufferMode
                    -> IO ()
setStdoutBufferMode mayBufferMode =
  hSetBuffering stdout (fromMaybe defaultStdoutMode mayBufferMode)
