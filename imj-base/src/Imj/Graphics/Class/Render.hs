{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.Render
    ( Render(..)
    -- * Reexports
    , module Imj.Graphics.Class.Draw
    , module Imj.Timing
    ) where

import           Control.Monad.IO.Class(MonadIO)

import           Imj.Graphics.Class.Draw
import           Imj.Timing

{- | Class describing the ability to render the result of a 'Draw' to the
screen.

It is left to the implementation to decide wether to clear the screen or not (after a
'renderToScreen' for example), and with which color. -}
class (Draw e) => Render e where
  -- | Render to the screen.
  --
  -- Returns:
  --
  -- * duration to compute delta
  -- * duration to issue rendering commands
  -- * duration to flush
  renderToScreen' :: (MonadIO m) => e -> m (Time Duration System, Time Duration System, Time Duration System)
