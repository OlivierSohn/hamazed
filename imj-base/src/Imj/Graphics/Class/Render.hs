{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.Render
    ( Render(..)
    ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Font
import           Imj.Timing

{- | Class describing the ability to render the result of a 'Draw' to the
screen.

It is left to the implementation to decide wether to clear the screen or not (after a
'renderToScreen' for example), and with which color. -}
class (Draw e) => Render e where
  -- | Change the font used to render text.
  cycleRenderingOptions' :: (MonadIO m) => e -> CycleFont -> CycleFontSize -> m (Either String ())
  applyPPUDelta :: (MonadIO m) => e -> PPU -> m (Either String ())
  applyFontMarginDelta :: (MonadIO m) => e -> FontMargin -> m (Either String ())

  -- | Render to the screen.
  --
  -- Returns:
  --
  -- * duration to compute delta
  -- * duration to issue rendering commands
  -- * duration to flush
  -- * Maybe the new buffers size
  renderToScreen' :: (MonadIO m) => e -> m (Maybe Size, Either String (Time Duration System, Time Duration System, Time Duration System))
