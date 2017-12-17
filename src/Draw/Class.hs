

module Draw.Class(
       -- * The Draw class
         Draw(..)
       -- * Reexports
       , ReaderT
       -- ** Colors
       , module Color
       -- ** Coordinates
       , module Geo.Discrete.Types
       ) where

import           Control.Monad.Reader(ReaderT)
import           Data.Text(Text)

import           Color
import           Color.Types
import           Geo.Discrete.Types

-- TODO how to make this generic and to allow any other monad transformer?
-- | Class of rendererers that will be used in a program through 'ReaderT'.
--
-- The renderers have the ability to draw colored text.
-- They should render to the physical destination in 'renderDrawing_'.
class Draw e where
  -- | Draw a char
  drawChar_ :: e -> (Char -> Coords -> LayeredColor -> ReaderT e IO ())
  drawChar_ = undefined

  -- | Draw repeated chars
  drawChars_ :: e -> (Int -> Char -> Coords -> LayeredColor -> ReaderT e IO ())
  drawChars_ = undefined

  -- | Draw 'Text'
  drawTxt_ :: e -> (Text -> Coords -> LayeredColor -> ReaderT e IO ())
  drawTxt_ = undefined

  -- | Render what was drawn
  renderDrawing_ :: e -> ReaderT e IO ()
  renderDrawing_ = undefined
