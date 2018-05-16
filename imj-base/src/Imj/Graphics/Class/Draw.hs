{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Graphics.Class.Draw
    ( Draw(..)
    , Scissor
    , Canvas
      -- * Reexports
    , Coords, Pos, LayeredColor, Glyph
    , HasRectArea
    ) where

import           Imj.Prelude

import           Data.Text(Text, unpack)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.UI.RectArea
import           Imj.Graphics.Class.Canvas
import           Imj.Graphics.Class.HasRectArea
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Font


type Scissor = RectArea (Filter Positive)

--'drawGlyphs'', 'drawTxt'' and 'drawStr'' could have been default-implemented in terms
--of 'drawGlyph', but the implementation would have been suboptimal in most cases.
{- | 'Draw' describes the ability to:

* draw colored 'Glyph's, 'String's, 'Text's,
* fill with a 'LayeredColor' and a 'Glyph',
* filter drawn locations using a 'Scissor'. -}
class (Canvas e) => Draw e where
  -- | Sets the area defining where to draw.
  --
  -- Do not use directly, prefer 'usingScissor'' instead.
  setScissor :: (MonadIO m) => e -> Scissor -> m ()

  -- | Gets the viewport defining where to draw.
  --
  -- Do not use directly, prefer 'usingScissor'' instead.
  getScissor' :: (MonadIO m) => e -> m Scissor

  -- | Fills the back buffer with the 'LayeredColor' (taking 'Scissor' into
  -- account if one is active)
  fill' :: (MonadIO m) => e -> Glyph -> LayeredColor -> m ()

  -- | Draw a 'Glyph'.
  drawGlyph' :: (MonadIO m) => e -> Glyph -> Coords Pos -> LayeredColor -> m ()
  -- | Draw repeated chars.
  drawGlyphs' :: (MonadIO m) => e -> Int -> Glyph -> Coords Pos -> LayeredColor -> m ()
  -- | Draw 'String'.
  drawStr' :: (MonadIO m) => e -> String -> Coords Pos -> LayeredColor -> m ()

  -- | Draw 'Text'.
  drawTxt' :: (MonadIO m) => e -> Text -> Coords Pos -> LayeredColor -> m ()
  drawTxt' e txt = drawStr' e (unpack txt)
  {-# INLINABLE drawTxt' #-}

  {- |
  1. Store the current 'Scissor'
  2. Execute the actions in the new 'Scissor'
  3. Restore the old 'Scissor'
  -}
  usingScissor' :: (MonadIO m, HasRectArea v)
                => e
                -> v
                -> m a
                -- ^ Actions
                -> m a
  usingScissor' env current action = do
    -- we need getScissor' to be in IO to be sure that it is executed before the rest:
    previous <- getScissor' env
    setScissor env (getRectArea current)
    res <- action
    setScissor env previous
    return res
