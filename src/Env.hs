module Env(
         Env(..)
       , createEnv
       , RenderFunctions(..)
       , Draw(..)
       -- * reexports
       ) where

import           Control.Monad.Reader(liftIO)
import           Draw.Class
import           Render.Delta

newtype Env = Env {
    _envRenderFuncs :: RenderFunctions
}

instance Draw Env where
  drawChar_  (Env (RenderFunctions f _ _ _)) c p co   = liftIO $ f c p co
  drawChars_ (Env (RenderFunctions _ f _ _)) i c p co = liftIO $ f i c p co
  drawTxt_   (Env (RenderFunctions _ _ f _)) t p co   = liftIO $ f t p co
  flush_     (Env (RenderFunctions _ _ _ f))          = liftIO f

createEnv :: IO Env
createEnv = Env . mkRenderFunctions <$> newDefaultContext
