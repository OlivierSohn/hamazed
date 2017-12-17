
-- | This can be used to wrap Render.Delta functions in a 'MonadReader'

module Draw.Class(
       -- * The Draw class
         Draw(..)
       -- * Reexports
       , Alignment(..)
       , MonadIO
       -- ** Colors
       , module Color.Types
       -- ** Coordinates
       , module Geo.Discrete.Types
       ) where

import           Prelude hiding(length)

import           Control.Monad.IO.Class(MonadIO)
import           Data.Text(Text, length)

import           Color.Types
import           Geo.Discrete
import           Geo.Discrete.Types
import           Text.Alignment


-- | Describes the ability to draw colored text on a drawing,
--  and to render the resulting drawing.
class Draw e where
  -- | Draw a 'Char'.
  drawChar_ :: (MonadIO m)
            => e
            -> (Char -> Coords -> LayeredColor -> m ())
  drawChar_ = undefined

  -- | Draw repeated chars.
  drawChars_ :: (MonadIO m)
             => e
             -> (Int -> Char -> Coords -> LayeredColor -> m ())
  drawChars_ = undefined

  -- | Draw 'Text'.
  drawTxt_ :: (MonadIO m)
           => e
           -> (Text -> Coords -> LayeredColor -> m ())
  drawTxt_ = undefined

  -- | Render the drawing to the physical destination.
  renderDrawing_ :: (MonadIO m) => e -> m ()
  renderDrawing_ = undefined


  -- | Draws text aligned w.r.t alignment and reference coodinates.
  {-# INLINABLE drawAlignedTxt'' #-}
  drawAlignedTxt'' :: (MonadIO m)
                   => e
                   -> Text
                   -> LayeredColor
                   -> Alignment
                   -> m ()
  drawAlignedTxt'' env txt colors a = do
    let leftCorner = align' a (length txt)
    drawTxt_ env txt leftCorner colors

  -- | Draws text aligned w.r.t alignment and reference coodinates.
  --
  -- Returns an 'Alignment' where the reference coordinate of the input 'Alignment' was projected on the next line.
  {-# INLINABLE drawAlignedTxt' #-}
  drawAlignedTxt' :: (MonadIO m)
                  => e
                  -> Text
                  -> LayeredColor
                  -> Alignment
                  -> m Alignment
  drawAlignedTxt' env txt colors a =
    drawAlignedTxt'' env txt colors a
      >> return (toNextLine a)
