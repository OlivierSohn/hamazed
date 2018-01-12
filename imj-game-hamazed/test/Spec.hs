import Control.Monad.Reader(runReaderT)

import           Imj.Graphics.Render
import           Imj.Graphics.Render.Delta(withDeltaRendering, DeltaRendering(..))

import           Imj.Game.Hamazed.Env

import           Test.Imj.Render

main :: IO ()
main = do
  putStrLn "" -- for readablilty

  withDeltaRendering Console $ \deltaEnv -> do
    let env = Env deltaEnv
    runReaderT (testSpace >> -- this is it visible in the logs, not in the console
                renderToScreen) env
