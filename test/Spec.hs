import Control.Monad.Reader(runReaderT)

import Env

import Render.Delta(restoreConsoleSettings)
import Draw

import Test.Rendering

main :: IO ()
main = do
  putStrLn "" -- for readablilty

  env <- createEnv
  runReaderT (testSpace >> -- TODO why is it visible in the logs, not in the console?
              renderDrawing
             ) env
  restoreConsoleSettings
