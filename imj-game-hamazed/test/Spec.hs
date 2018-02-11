import           Control.Monad.Reader(runReaderT)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Graphics.Render
import           Imj.Graphics.Render.Delta
import           Imj.Game.Hamazed.Env
import           Imj.Game.Hamazed.Network.GameNode
import           Imj.Game.Hamazed.Network.Server

import           Test.Imj.Render

main :: IO ()
main = do
  putStrLn "" -- for readablilty

  newConsoleBackend >>= \backend ->
    flip withDefaultPolicies backend (\deltaEnv -> do
      env <- Env deltaEnv backend <$> startNetworking (SuggestedPlayerName "Me") (mkServer Nothing defaultPort)
      _ <- runReaderT (testSpace >> -- this is it visible in the logs, not in the console
                  renderToScreen) env
      return ())
