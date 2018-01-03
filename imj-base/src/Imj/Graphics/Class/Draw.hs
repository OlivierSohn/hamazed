{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.Draw
    ( Draw(..)
    , Scissor
      -- * Reexports
    , Coords, Pos, LayeredColor
    , MonadIO
    , HasRectArea
    ) where

import           Imj.Prelude

import           Control.Monad(foldM_)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Text(Text, length)

import           Imj.Geo.Discrete
import           Imj.Graphics.UI.RectArea
import           Imj.Graphics.Class.HasRectArea
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Text.Alignment
import           Imj.Graphics.Text.ColorString


type Scissor = RectArea (Filter Positive)

--'drawChars'', 'drawTxt'' and 'drawStr'' could have been default-implemented in terms
--of 'drawChar', but the implementation would have been suboptimal in most cases.
{- | 'Draw' describes the ability to:

* draw colored 'Char's, 'String's, 'Text's,
* fill with a 'LayeredColor' and a 'Char',
* filter drawn locations using a 'Scissor'. -}
class Draw e where
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
  fill' :: (MonadIO m) => e -> Char -> LayeredColor -> m ()

  -- | Draw a 'Char'.
  drawChar' :: (MonadIO m) => e -> Char -> Coords Pos -> LayeredColor -> m ()
  -- | Draw repeated chars.
  drawChars' :: (MonadIO m) => e -> Int -> Char -> Coords Pos -> LayeredColor -> m ()
  -- | Draw 'Text'.
  drawTxt' :: (MonadIO m) => e -> Text -> Coords Pos -> LayeredColor -> m ()
  -- | Draw 'String'.
  drawStr' :: (MonadIO m) => e -> String -> Coords Pos -> LayeredColor -> m ()

  {- |
  1. Store the current 'Scissor'
  2. Execute the actions in the new 'Scissor'
  3. Restore the old 'Scissor'
  -}
  usingScissor' :: (MonadIO m, HasRectArea v)
                => e
                -> v
                -> m ()
                -- ^ Actions
                -> m ()
  usingScissor' env current action = do
    -- we need getScissor' to be in IO to be sure that it is executed before the rest:
    previous <- getScissor' env
    setScissor env (getRectArea current)
    action
    setScissor env previous

  -- | Draw a 'ColorString'.
  {-# INLINABLE drawColorStr' #-}
  drawColorStr' :: (MonadIO m) => e -> ColorString -> Coords Pos -> m ()
  drawColorStr' env (ColorString cs) pos =
    foldM_
      (\count (txt, color) -> do
        let l = length txt
        drawTxt' env txt (move count RIGHT pos) color
        return $ count + l
      ) 0 cs

  -- | Draw text aligned w.r.t alignment and reference coordinates.
  {-# INLINABLE drawAlignedTxt_' #-}
  drawAlignedTxt_' :: (MonadIO m) => e -> Text -> LayeredColor -> Alignment -> m ()
  drawAlignedTxt_' env txt colors a = do
    let leftCorner = align' a (length txt)
    drawTxt' env txt leftCorner colors

  -- | Draws text aligned w.r.t alignment and reference coordinates.
  --
  -- Returns an 'Alignment' where the reference coordinate of the input 'Alignment'
  -- was projected on the next line.
  {-# INLINABLE drawAlignedTxt' #-}
  drawAlignedTxt' :: (MonadIO m) => e -> Text -> LayeredColor -> Alignment -> m Alignment
  drawAlignedTxt' env txt colors a =
    drawAlignedTxt_' env txt colors a
      >> return (toNextLine a)

  -- | Draw a 'ColorString' with an 'Alignment' constraint.
  {-# INLINABLE drawAlignedColorStr' #-}
  drawAlignedColorStr' :: (MonadIO m)  => e -> Alignment -> ColorString -> m Alignment
  drawAlignedColorStr' env a cs = do
    let leftCorner = align' a (countChars cs)
    _ <- drawColorStr' env cs leftCorner
    return $ toNextLine a
