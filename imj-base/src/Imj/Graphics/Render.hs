{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Render
  (
  -- * Types for Draw and Render
  {- | 'Draw' does not necessarily render to the screen, but 'Render' makes the
  result of 'Draw' method calls visible on the screen.

  Optimized instances of 'Draw' and 'Render', for games and animations
  drawing in the terminal, are
  available in "Imj.Graphics.Render.Delta.Env". They minimize stdout usage using double
  buffering and delta rendering, thereby mitigating the
  <https://en.wikipedia.org/wiki/Screen_tearing screen tearing effect>.
  -}
    Scissor
  , Draw(..)
  , Render(..)
  -- * From MonadReader
{- | The functions below use 'Draw', 'Canvas', 'Render' instances in a 'MonadReader' monad.

Hence, if you run in a 'MonadReader' 'YourEnv' monad
(where 'YourEnv' is your environment equiped with 'Draw', 'Canvas' and 'Render' instances),
you can write:

@
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Reader.Class(MonadReader)
import Control.Monad.Reader(runReaderT)

import Imj.Graphics.Class.Render
import Imj.Graphics.Render.FromMonadReader(drawStr, renderToScreen)

helloWorld :: (Draw e, Render e, MonadReader e m, MonadIO m) => m ()
helloWorld = drawStr \"Hello World\" (Coords 10 10) green >> renderToScreen

main = createEnv >>= runReaderT helloWorld
@

<https://github.com/OlivierSohn/hamazed/blob/f38901ba9e33450cae1425c26fd55bd7b171c5ba/imj-game-hamazed/src/Imj/Game/Hamazed/Env.hs This example>
follows this pattern. -}
  -- * Use Scissor
  , usingScissor
  -- * Draw colored chars
  , fill
  , drawGlyph
  , drawGlyphs
  , drawTxt
  , drawStr
  , drawMultiLineStr
  -- ** Render to the physical device
  , renderToScreen
  -- * Reexports
  , LayeredColor(..), Coords(..), Pos
  , Alignment(..), Text, Char, String
  , MonadReader, MonadIO
  ) where

import           Imj.Prelude

import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Text(Text)

import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Render
import           Imj.Graphics.Render.FromMonadReader

import           Imj.Graphics.Color(LayeredColor(..))
import           Imj.Geo.Discrete(Coords(..))
import           Imj.Graphics.Text.Alignment(Alignment(..))
