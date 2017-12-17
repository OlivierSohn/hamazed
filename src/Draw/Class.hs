
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
--
-- Functions are postfixed with @'@: non-postfixed names are reserved
-- to helper functions in "Draw.Helpers.MonadReader", because using the API through the
-- 'MonadReader' monad is the recommended way.
class Draw e where
  -- | Draw a 'Char'.
  drawChar' :: (MonadIO m)
            => e
            -> (Char -> Coords -> LayeredColor -> m ())
  drawChar' = undefined

  -- | Draw repeated chars.
  drawChars' :: (MonadIO m)
             => e
             -> (Int -> Char -> Coords -> LayeredColor -> m ())
  drawChars' = undefined

  -- | Draw 'Text'.
  drawTxt' :: (MonadIO m)
           => e
           -> (Text -> Coords -> LayeredColor -> m ())
  drawTxt' = undefined

  -- | Render the drawing to the physical destination.
  renderDrawing' :: (MonadIO m) => e -> m ()
  renderDrawing' = undefined


  -- | Draws text aligned w.r.t alignment and reference coodinates.
  {-# INLINABLE drawAlignedTxt_' #-}
  drawAlignedTxt_' :: (MonadIO m)
                   => e
                   -> Text
                   -> LayeredColor
                   -> Alignment
                   -> m ()
  drawAlignedTxt_' env txt colors a = do
    let leftCorner = align' a (length txt)
    drawTxt' env txt leftCorner colors

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
    drawAlignedTxt_' env txt colors a
      >> return (toNextLine a)
