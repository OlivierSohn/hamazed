module Env(
         Env
       , createEnv
       ) where

import           Control.Monad.Reader(liftIO)
import           Draw.Class
import           Render.Delta


-- | The environment of <https://github.com/OlivierSohn/hamazed Hamazed> program,
--  which will be accessed through a 'ReaderT' monad transformer.
newtype Env = Env {
    _envRenderFuncs :: RenderFunctions
    -- ^ Allows to implement the 'Draw' instance
}

instance Draw Env where
  drawChar_      (Env (RenderFunctions f _ _ _)) c p co   = liftIO $ f c p co
  drawChars_     (Env (RenderFunctions _ f _ _)) i c p co = liftIO $ f i c p co
  drawTxt_       (Env (RenderFunctions _ _ f _)) t p co   = liftIO $ f t p co
  renderDrawing_ (Env (RenderFunctions _ _ _ f))          = liftIO f

-- | Contructor of 'Env'
createEnv :: IO Env
createEnv = Env . mkRenderFunctions <$> newDefaultContext
