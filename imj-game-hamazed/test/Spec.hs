import Control.Monad.Reader(runReaderT)

import           Imj.Draw
import           Imj.Env
import           Imj.Render.Delta(runThenRestoreConsoleSettings)

import           Test.Imj.Rendering

main :: IO ()
main = do
  putStrLn "" -- for readablilty

  runThenRestoreConsoleSettings $
    createEnv
      >>= runReaderT (testSpace >> -- this is it visible in the logs, not in the console
                      renderDrawing)
