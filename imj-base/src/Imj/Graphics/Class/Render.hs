{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.Render(
         Render(..)
       ) where

import           Control.Monad.IO.Class(MonadIO)

import           Imj.Graphics.Class.Draw

{- | Class describing the ability to render the result of a 'Draw' to the
screen.

It is left to the implementation to decide wether to clear the screen or not (after a
'renderToScreen' for example), and with which color. -}
class (Draw e) => Render e where
  -- | Render to the screen.
  renderToScreen' :: (MonadIO m) => e -> m ()
