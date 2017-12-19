
module Draw.Class(
       -- * The Draw class
         Draw(..)
       -- * Reexports
       , MonadIO
       , Text
       -- ** Colors
       , module Color.Types
       -- ** Coordinates
       , module Geo.Discrete.Types
       ) where

import           Control.Monad.IO.Class(MonadIO)
import           Data.Text(Text)

import           Color.Types
import           Geo.Discrete.Types


{- | Describes the ability to draw colored text on a drawing,
 and to render the resulting drawing.

Functions are postfixed with @'@: non-postfixed names are reserved
to helper functions in "Draw.Helpers.MonadReader", because using the API through the
'MonadReader' monad is the recommended way.

'drawChars\'' and 'drawTxt\'' could have been default-implemented in terms of 'drawChar\'',
but I thought that in most case it will be easy to optimize for these cases,
so I don't want to provide a suboptimal implementation.
-}
class Draw e where
  -- | Draw a 'Char'.
  drawChar' :: (MonadIO m)
            => e
            -> Char
            -> Coords
            -> LayeredColor
            -> m ()
  drawChar' = undefined

  -- | Draw repeated chars.
  drawChars' :: (MonadIO m)
             => e
             -> Int -> Char -> Coords -> LayeredColor -> m ()
  drawChars' = undefined

  -- | Draw 'Text'.
  drawTxt' :: (MonadIO m)
           => e
           -> Text -> Coords -> LayeredColor -> m ()
  drawTxt' = undefined

  -- | Render the drawing to the physical destination.
  renderDrawing' :: (MonadIO m) => e -> m ()
  renderDrawing' = undefined
