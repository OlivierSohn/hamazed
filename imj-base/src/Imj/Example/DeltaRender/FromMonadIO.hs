{-# OPTIONS_HADDOCK hide #-}

-- | This example is related to "Imj.Graphics.Render.Delta" : /from a 'MonadIO'/

module Imj.Example.DeltaRender.FromMonadIO
  ( main
  ) where

import Imj.Prelude

import Imj.Graphics.Color
import Imj.Graphics.Render.Delta

helloWorld :: (MonadIO m) => DeltaEnv -> m ()
helloWorld env = do
  drawStr' env "Hello World" (Coords 10 10) (onBlack green)
  void $ renderToScreen' env


main :: IO ()
main =
  newConsoleBackend >>= withDefaultPolicies helloWorld
