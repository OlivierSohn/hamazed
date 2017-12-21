{-# LANGUAGE NoImplicitPrelude #-}

-- | This module exports functions to create and configure a 'DeltaEnv'.

module Render.Delta.Env
          (
          -- * Type
            DeltaEnv
          -- * Creation
          , newDefaultEnv
          , newEnv
          -- * Configuration
          --
          -- | Policy changes will be applied only after the next render:
          --
          --     * 'setResizePolicy' lets you chose how front and back buffer resize:
          --     they can either dynamically adapt to the terminal size, or have a fixed size.
          --
          --     * 'setClearPolicy' and 'setClearColor' let you configure if and how the back buffer
          --      is cleared after a render.
          --
          --     * Finally, 'setStdoutBufferMode' lets you to chose stdout's 'BufferMode'.

          , setResizePolicy
          , defaultResizePolicy
          , ResizePolicy(..)
          , setClearPolicy
          , defaultClearPolicy
          , ClearPolicy(..)
          , setClearColor
          , defaultClearColor
          , setStdoutBufferMode
          , defaultStdoutMode
          -- * Reexports
          , BufferMode(..)
          ) where

import           Imajuscule.Prelude

import           System.IO(BufferMode(..), hSetBuffering, stdout)

import           Control.Monad.IO.Class(liftIO)

import           Data.IORef( IORef, readIORef, writeIORef )
import           Data.Maybe( fromMaybe )

import           Draw.Class

import           Render.Delta.Buffers
import           Render.Delta.Console
import           Render.Delta.Draw
import           Render.Delta.Flush
import           Render.Delta.DefaultPolicies
import           Render.Delta.Types

newtype DeltaEnv = DeltaEnv (IORef Buffers)

-- | Draws and renders using the delta rendering engine.
instance Draw DeltaEnv where
  drawChar'      (DeltaEnv a) b c d   = liftIO $ deltaDrawChar  a b c d
  drawChars'     (DeltaEnv a) b c d e = liftIO $ deltaDrawChars a b c d e
  drawTxt'       (DeltaEnv a) b c d   = liftIO $ deltaDrawTxt   a b c d
  drawStr'       (DeltaEnv a) b c d   = liftIO $ deltaDrawStr   a b c d
  renderDrawing' (DeltaEnv a)         = liftIO $ deltaFlush     a
  {-# INLINABLE drawChar' #-}
  {-# INLINABLE drawChars' #-}
  {-# INLINABLE drawTxt' #-}
  {-# INLINABLE renderDrawing' #-}


-- | Creates an environment using default policies (see "Render.Delta.DefaultPolicies").
newDefaultEnv :: IO DeltaEnv
newDefaultEnv = newEnv Nothing Nothing Nothing Nothing

newEnv :: Maybe ResizePolicy
       -> Maybe ClearPolicy
       -> Maybe (Color8 Background)
       -> Maybe BufferMode
       -- ^ Preferred stdout buffering.
       -> IO DeltaEnv
newEnv a b c mayBufferMode = do
  let stdoutBufMode = fromMaybe defaultStdoutMode mayBufferMode
  configureConsoleFor Gaming stdoutBufMode
  DeltaEnv <$> newContext a b c


-- | Sets the 'ResizePolicy' for front and back buffers.
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

-- | Sets 'ClearColor' to use when clearing.
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
