{-# OPTIONS_HADDOCK hide #-}

-- | This example is related to "Imj.Graphics.Render.Delta" :
-- /from a 'MonadIO', 'MonadReader' 'DeltaEnv' monad/

module Imj.Example.DeltaRender.FromMonadReader
  ( main
  ) where

import Control.Monad.Reader(runReaderT, void)
import Control.Monad.IO.Class(MonadIO)

import Imj.Graphics.Color
import Imj.Graphics.Render.Delta

helloWorld :: (Render e, MonadReader e m, MonadIO m) => m ()
helloWorld = do
  drawStr "Hello World" (Coords 10 10) (onBlack green)
  void renderToScreen


main :: IO ()
main =
  newConsoleBackend >>= withDefaultPolicies (runReaderT helloWorld)
