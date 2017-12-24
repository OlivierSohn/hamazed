{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Draw.Class(
         Draw(..)
       ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Data.Text(Text)

import           Imj.Color.Types
import           Imj.Geo.Discrete.Types


{- | Class describing the ability to draw colored text on a drawing,
 and to render the resulting drawing.
-}
--'drawChars'', 'drawTxt'' and 'drawStr'' could have been default-implemented in terms
--of 'drawChar', but the implementation would have been suboptimal in most cases.
class Draw e where
  -- | Draw a 'Char'.
  drawChar' :: (MonadIO m)
            => e
            -> Char
            -> Coords
            -> LayeredColor
            -> m ()

  -- | Draw repeated chars.
  drawChars' :: (MonadIO m)
             => e
             -> Int -> Char -> Coords -> LayeredColor -> m ()

  -- | Draw 'Text'.
  drawTxt' :: (MonadIO m)
           => e
           -> Text -> Coords -> LayeredColor -> m ()

  -- | Draw 'String'.
  drawStr' :: (MonadIO m)
           => e
           -> String -> Coords -> LayeredColor -> m ()

  -- | Render the drawing to the physical destination.
  renderDrawing' :: (MonadIO m) => e -> m ()

  -- TODO put ColorString and Aligned rendering in this class
