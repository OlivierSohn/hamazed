

module Render.Draw(
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

-- | Instances of this class can render colored text in the console,
-- and will be argument of a ReaderT.
class Draw e where
  drawChar_ :: e -> (Char -> Coords -> LayeredColor -> ReaderT e IO ())
  drawChar_ = undefined

  drawChars_ :: e -> (Int -> Char -> Coords -> LayeredColor -> ReaderT e IO ())
  drawChars_ = undefined

  drawTxt_ :: e -> (Text -> Coords -> LayeredColor -> ReaderT e IO ())
  drawTxt_ = undefined

  flush_ :: e -> ReaderT e IO ()
  flush_ = undefined
