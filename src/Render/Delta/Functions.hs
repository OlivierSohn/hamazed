{-# OPTIONS_HADDOCK hide #-}

module Render.Delta.Functions
            ( -- * Abstracting away 'IORef' 'Buffers'
              mkRenderFunctions
            , RenderFunctions(..)
            )where

import           Data.Text(Text)

import           Render.Delta.Draw
import           Render.Delta.Flush
import           Render.Delta.Internal.Types

-- | Abstracts away 'IORef' 'Buffers', by preapplying it.
--
-- Then, you can store
-- this in your environment, and access it through 'ReaderT', cf "Draw".
mkRenderFunctions :: IORef Buffers -> RenderFunctions
mkRenderFunctions b = RenderFunctions (drawChar b) (drawChars b) (drawTxt b) (flush b)

-- | Contains the functions of "Render.Delta.Draw" where the 'IORef' 'Buffers'
--   has been preapplied.
data RenderFunctions = RenderFunctions {
    _renderChar :: !(Char -> Coords -> LayeredColor -> IO ())
  , _renderChars :: !(Int -> Char -> Coords -> LayeredColor -> IO ())
  , _renderTxt :: !(Text -> Coords -> LayeredColor -> IO ())
  , _flush :: !(IO())
}
