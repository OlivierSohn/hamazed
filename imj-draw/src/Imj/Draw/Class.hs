{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Draw.Class(
       -- * The Draw class
         Draw(..)
       -- * Reexports
       -- ** Colors
       , module Imj.Color.Types
       -- ** Coordinates
       , module Imj.Geo.Discrete.Types
       -- ** Reexports
       , MonadIO
       , Text
       ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Data.Text(Text)

import           Imj.Color.Types
import           Imj.Geo.Discrete.Types


{- | Describes the ability to draw colored text on a drawing,
 and to render the resulting drawing.

Functions are postfixed with @'@. Non-postfixed names are reserved
to helper functions in "Draw.Helpers.MonadReader" (using the API through a
'MonadReader' monad is the recommended way).

'drawChars', 'drawTxt' and 'drawStr' could have been default-implemented in terms
of 'drawChar', but the implementation would have been suboptimal in most cases.
-}
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

  drawStr' :: (MonadIO m)
           => e
           -> String -> Coords -> LayeredColor -> m ()

  -- | Render the drawing to the physical destination.
  renderDrawing' :: (MonadIO m) => e -> m ()
