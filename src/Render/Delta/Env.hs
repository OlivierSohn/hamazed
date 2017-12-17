{- | This module exports functions related to the creation and configuration
of the delta rendering environment 'DeltaEnv'.

Some policies ('ClearPolicy', 'ResizePolicy') /can/ be specified, however default
policies cover most use cases.
-}

module Render.Delta.Env
          (
          -- * Environment
          -- ** Creation
            newDefaultEnv
          , newEnv
          , DeltaEnv(..)
          -- ** Configuration
          -- *** Resize policy
          , setResizePolicy
          , ResizePolicy(..)
          -- *** Clear policy (back-buffer)
          , setClearPolicy
          , ClearPolicy(..)
          -- *** Clear color (back-buffer)
          , setClearColor
          , ClearColor
          -- *** Stdout
          , setStdoutBufferMode
          -- * Restore console settings
          , restoreConsoleSettings
          -- * Reexports
          , BufferMode(..)
          ) where

import           System.IO(BufferMode(..), hSetBuffering, stdout)

import           Control.Monad.IO.Class(liftIO)

import           Data.IORef( IORef, readIORef, writeIORef )
import           Data.Maybe( fromMaybe )

import           Draw.Class

import           Render.Delta.Buffers
import           Render.Delta.Console
import           Render.Delta.Draw
import           Render.Delta.Flush
import           Render.Delta.Internal.DefaultPolicies
import           Render.Delta.Types

newtype DeltaEnv = DeltaEnv (IORef Buffers)

-- | Draw and render using delta rendering engine.
instance Draw DeltaEnv where
  drawChar'      (DeltaEnv a) b c d   = liftIO $ deltaDrawChar  a b c d
  drawChars'     (DeltaEnv a) b c d e = liftIO $ deltaDrawChars a b c d e
  drawTxt'       (DeltaEnv a) b c d   = liftIO $ deltaDrawTxt   a b c d
  renderDrawing' (DeltaEnv a)         = liftIO $ deltaFlush     a
  {-# INLINABLE drawChar' #-}
  {-# INLINABLE drawChars' #-}
  {-# INLINABLE drawTxt' #-}
  {-# INLINABLE renderDrawing' #-}


-- | Creates an environment with default values that are sensible for most use cases.
--
-- It is equivalent to @newEnv Nothing Nothing Nothing Nothing@.
newDefaultEnv :: IO DeltaEnv
newDefaultEnv = newEnv Nothing Nothing Nothing Nothing

-- | Creates a delta rendering environment using optional policies.
newEnv :: Maybe ResizePolicy
       -> Maybe ClearPolicy
       -> Maybe ClearColor
       -> Maybe BufferMode
       -- ^ Preferred stdout buffering (default will be optimal in most cases).
       -> IO DeltaEnv
newEnv a b c mayBufferMode = do
  let stdoutBufMode = fromMaybe defaultStdoutMode mayBufferMode
  configureConsoleFor Gaming stdoutBufMode
  DeltaEnv <$> newContext a b c


-- | Sets an optional 'ResizePolicy' for front and back buffers.
-- Uses the default policy when 'Nothing' is passed.
--
-- If needed, a resize will occur at the end of the next rendering.
setResizePolicy :: Maybe ResizePolicy -> DeltaEnv -> IO ()
setResizePolicy mayResizePolicy (DeltaEnv ref) =
  readIORef ref
    >>= \(Buffers a b d e (Policies _ f g)) -> do
      let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
      writeIORef ref $ Buffers a b d e (Policies resizePolicy f g)


-- | Sets 'ClearPolicy' to use for an environment.
--   Default policy is used when Nothing is passed.
setClearPolicy :: Maybe ClearPolicy -> DeltaEnv -> IO ()
setClearPolicy mayClearPolicy (DeltaEnv ref) =
  readIORef ref
    >>= \(Buffers a b d e (Policies f _ clearColor)) -> do
      let clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
          buffers = Buffers a b d e (Policies f clearPolicy clearColor)
      writeIORef ref buffers

-- | Sets 'ClearColor' to use for an environment.
--   Default color (black) is used when Nothing is passed.
setClearColor :: Maybe ClearColor -> DeltaEnv -> IO ()
setClearColor mayClearColor (DeltaEnv ref) =
  readIORef ref
    >>= \(Buffers a b d e (Policies f clearPolicy _)) -> do
      let clearColor = fromMaybe defaultClearColor mayClearColor
          buffers = Buffers a b d e (Policies f clearPolicy clearColor)
      writeIORef ref buffers

-- | The default value (Nothing) maximizes the size stdout buffer.
setStdoutBufferMode :: Maybe BufferMode -> IO ()
setStdoutBufferMode mayBufferMode =
  hSetBuffering stdout (fromMaybe defaultStdoutMode mayBufferMode)


-- | Restores stdin, stdout bufferings, unhides the cursor, restores echo for
--   stdin, restores the buffering of stdout to 'LineBuffering'
--
-- Typically, this function should be called before your game exits, to restore
-- common standard settings in the console.
restoreConsoleSettings :: IO ()
restoreConsoleSettings =
 configureConsoleFor Editing LineBuffering
