module Imj.Example.DeltaRenderFromMonadIO
  ( main
  ) where

import Control.Monad.IO.Class(MonadIO, liftIO)

import Imj.Graphics.Color
import Imj.Graphics.Class.Draw(drawStr')
import Imj.Graphics.Class.Render(renderToScreen')
import Imj.Graphics.Render.Delta

helloWorld :: (MonadIO m) => DeltaEnv -> m ()
helloWorld env = do
  drawStr' env "Hello World" (Coords 10 10) (onBlack green)
  renderToScreen' env

main :: (MonadIO m) => m ()
main = liftIO (runThenRestoreConsoleSettings $ newDefaultEnv) >>= helloWorld
