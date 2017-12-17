
module Env(
         Env
       , createEnv
       ) where

import           Control.Monad.Reader(liftIO)

import           Draw.Class
import           Render.Delta


-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program
--
-- In this program we will access 'Env' using the 'ReaderT' 'Env' 'IO' monad.
-- This monad is compatible with 'Draw' functions constraints which are: (Draw e, MonadIO m)
newtype Env = Env {
    _envRenderBuffers :: IORef Buffers
}

-- | The 'Draw' instance of 'Env' wrap Render.Delta functions.
instance Draw Env where
  drawChar_      (Env a) b c d   = liftIO $ drawChar  a b c d
  drawChars_     (Env a) b c d e = liftIO $ drawChars a b c d e
  drawTxt_       (Env a) b c d   = liftIO $ drawTxt   a b c d
  renderDrawing_ (Env a)         = liftIO $ flush     a

-- | Contructor of 'Env'
createEnv :: IO Env
createEnv = Env <$> newDefaultContext
