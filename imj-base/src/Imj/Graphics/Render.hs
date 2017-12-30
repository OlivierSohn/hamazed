{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Render
  (
  -- * Draw and Render
  {- | 'Draw' describes the ability to draw colored 'Char's, 'String's, 'Text's.

  'Render' makes the result of draw*** calls visible on the screen.

  Optimized instances of 'Draw' and 'Render', for games and animations
  drawing in the terminal, are
  available in "Imj.Graphics.Render.Delta.Env". They minimize stdout usage using double
  buffering and delta rendering, thereby mitigating the
  <https://en.wikipedia.org/wiki/Screen_tearing screen tearing effect>.
  -}
    module Imj.Graphics.Class.Draw
  , module Imj.Graphics.Class.Render
  -- * From MonadReader
{- | The functions below use 'Draw' and 'Render' instances in a 'MonadReader' monad.

Hence, if you run in a 'MonadReader' 'YourEnv' monad
(where 'YourEnv' is your environment equiped with 'Draw' and 'Render' instances),
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

<https://github.com/OlivierSohn/hamazed/blob/master/imj-game-hamazed/src/Imj/Game/Hamazed/Env.hs This example>
follows this pattern. -}
  , module Imj.Graphics.Render.FromMonadReader
  -- * Reexports
  , LayeredColor(..)
  , Coords(..)
  , Alignment(..)
  , Text
  , Char
  , String
  , MonadIO
  , MonadReader
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
