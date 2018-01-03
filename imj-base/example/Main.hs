
{- | This application runs in the terminal, and shows animated text examples.

In particular, it shows examples using these functions:

* 'mkSequentialTextTranslationsCharAnchored' / 'drawAnimatedTextCharAnchored'
* 'mkSequentialTextTranslationsStringAnchored' / 'drawAnimatedTextStringAnchored'
-}

module Main where

import           Control.Monad.Reader(runReaderT)

import           Imj.Graphics.Render.Delta(runThenRestoreConsoleSettings, newDefaultEnv)

import           Imj.Example.SequentialTextTranslationsAnchored

main :: IO ()
main = do
  runThenRestoreConsoleSettings $
    newDefaultEnv
      >>= runReaderT exampleOfsequentialTextTranslationsAnchored
