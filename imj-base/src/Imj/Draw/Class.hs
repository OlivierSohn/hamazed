{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Draw.Class(
         Draw(..)
       ) where

import           Imj.Prelude

import           Control.Monad(foldM_)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Text(Text, length)

import           Imj.Color.Types
import           Imj.Geo.Discrete
import           Imj.Text.Alignment
import           Imj.Text.ColorString


{- | Class describing the ability to draw colored text on a drawing,
 and to render the resulting drawing.
-}
--'drawChars'', 'drawTxt'' and 'drawStr'' could have been default-implemented in terms
--of 'drawChar', but the implementation would have been suboptimal in most cases.
class Draw e where
  -- | Draw a 'Char'.
  drawChar' :: (MonadIO m) => e -> Char -> Coords -> LayeredColor -> m ()
  -- | Draw repeated chars.
  drawChars' :: (MonadIO m) => e -> Int -> Char -> Coords -> LayeredColor -> m ()
  -- | Draw 'Text'.
  drawTxt' :: (MonadIO m) => e -> Text -> Coords -> LayeredColor -> m ()
  -- | Draw 'String'.
  drawStr' :: (MonadIO m) => e -> String -> Coords -> LayeredColor -> m ()

  -- | Draw a 'ColorString'.
  {-# INLINABLE drawColorStr' #-}
  drawColorStr' :: (MonadIO m) => e -> ColorString -> Coords -> m ()
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

  -- | Render the drawing to the physical destination.
  renderDrawing' :: (MonadIO m) => e -> m ()
