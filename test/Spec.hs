import Control.Monad.Reader(runReaderT)

import Draw
import Env
import Render.Delta(runThenRestoreConsoleSettings)

import Test.Rendering

main :: IO ()
main = do
  putStrLn "" -- for readablilty

  runThenRestoreConsoleSettings $
    createEnv
      >>= runReaderT (testSpace >> -- this is it visible in the logs, not in the console
                      renderDrawing)
