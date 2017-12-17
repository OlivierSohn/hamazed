module Render.Delta.Env
          ( DeltaEnv(..)
          ) where

import           Control.Monad.IO.Class(liftIO)

import           Draw.Class

import           Render.Delta.Types
import           Render.Delta.Draw
import           Render.Delta.Flush

newtype DeltaEnv = DeltaEnv (IORef Buffers)

-- | Wraps Delta functions.
instance Draw DeltaEnv where
  drawChar_      (DeltaEnv a) b c d   = liftIO $ drawChar  a b c d
  drawChars_     (DeltaEnv a) b c d e = liftIO $ drawChars a b c d e
  drawTxt_       (DeltaEnv a) b c d   = liftIO $ drawTxt   a b c d
  renderDrawing_ (DeltaEnv a)         = liftIO $ flush     a
  {-# INLINABLE drawChar_ #-}
  {-# INLINABLE drawChars_ #-}
  {-# INLINABLE drawTxt_ #-}
  {-# INLINABLE renderDrawing_ #-}
