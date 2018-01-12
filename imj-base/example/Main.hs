
{- | This application runs in the terminal, and shows animated text examples.

In particular, it shows examples using these functions:

* 'mkSequentialTextTranslationsCharAnchored' / 'drawAnimatedTextCharAnchored'
* 'mkSequentialTextTranslationsStringAnchored' / 'drawAnimatedTextStringAnchored'
-}

module Main where

import           Control.Monad.Reader(runReaderT)

import           Imj.Graphics.Render.Delta(withDeltaRendering, DeltaRendering(..))

import           Imj.Example.SequentialTextTranslationsAnchored

main :: IO ()
main =
  withDeltaRendering Console (runReaderT exampleOfsequentialTextTranslationsAnchored)
