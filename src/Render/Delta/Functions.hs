module Render.Delta.Functions
            ( RenderFunctions(..)
            , mkRenderFunctions
            )where

import           Data.Text(Text)

import           Render.Delta.Draw
import           Render.Delta.Flush
import           Render.Delta.Types

data RenderFunctions = RenderFunctions {
    _renderChar :: !(Char -> Coords -> LayeredColor -> IO ())
  , _renderChars :: !(Int -> Char -> Coords -> LayeredColor -> IO ())
  , _renderTxt :: !(Text -> Coords -> LayeredColor -> IO ())
  , _flush :: !(IO())
}

mkRenderFunctions :: IORef Buffers -> RenderFunctions
mkRenderFunctions b = RenderFunctions (drawChar b) (drawChars b) (drawTxt b) (flush b)
