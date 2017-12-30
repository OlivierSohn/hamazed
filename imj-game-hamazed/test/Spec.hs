import Control.Monad.Reader(runReaderT)

import           Imj.Graphics.Render
import           Imj.Graphics.Render.Delta(runThenRestoreConsoleSettings)

import           Imj.Game.Hamazed.Env

import           Test.Imj.Render

main :: IO ()
main = do
  putStrLn "" -- for readablilty

  runThenRestoreConsoleSettings $
    createEnv
      >>= runReaderT (testSpace >> -- this is it visible in the logs, not in the console
                      renderToScreen)
