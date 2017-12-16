

module Draw.Class(
       -- * The Draw class
         Draw(..)
       , ReaderT
       -- * Types
       -- ** Colors
       , module Color -- this is hidden because it's big
       , module Color.Types
       -- ** Coordinates
       , module Geo.Discrete.Types
       ) where

import           Control.Monad.Reader(ReaderT)
import           Data.Text(Text)

import           Color
import           Color.Types
import           Geo.Discrete.Types

-- TODO how to make this generic and to allow any other monad transformer?
-- | Class of rendererers that will be used through ReaderT.
--
-- They can draw colored text on a backbuffer, and they render it on 'flush_'.
class Draw e where
  -- | Draw a char on the backbuffer
  drawChar_ :: e -> (Char -> Coords -> LayeredColor -> ReaderT e IO ())
  drawChar_ = undefined

  -- | Draw repeated chars on the backbuffer
  drawChars_ :: e -> (Int -> Char -> Coords -> LayeredColor -> ReaderT e IO ())
  drawChars_ = undefined

  -- | Draw 'Text' on the backbuffer
  drawTxt_ :: e -> (Text -> Coords -> LayeredColor -> ReaderT e IO ())
  drawTxt_ = undefined

  -- | Render the backbuffer
  flush_ :: e -> ReaderT e IO ()
  flush_ = undefined
