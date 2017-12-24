{-# LANGUAGE NoImplicitPrelude #-}

{- |

The 'MonadReader' functions provide a way to abstract a global renderer, in a style adhering to
<https://www.fpcomplete.com/blog/2017/06/readert-design-pattern these recommendations>
regarding global state in a program.

As a user of these modules, you will run your program in a 'MonadReader' 'YourEnv' monad,
where 'YourEnv' is your environment equiped with a 'Draw' instance.

Then, you can simply write:

@
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Reader.Class(MonadReader)
import Control.Monad.Reader(runReaderT)

import Imj.Draw.Class
import Imj.Draw.Helpers.MonadReader(drawTxt, renderDrawing)

helloWorld :: (Draw e, MonadReader e m, MonadIO m)
           => m ()
helloWorld = do
  drawStr \"Hello\" (Coords 10 10) red
  drawStr \"World\" (Coords 20 20) green
  renderDrawing

main = do
  env <- createEnv
  runReaderT helloWorld env
@

<https://github.com/OlivierSohn/hamazed/blob/master/imj-game-hamazed/imj-game-hamazed/src/Imj.Env.hs This example>
follows this pattern.

-}

module Imj.Draw
        (
        -- * From MonadIO
          module Imj.Draw.Class
        -- * From MonadReader
        -- | These functions contain the boilerplate code to call the
        -- 'Draw' functions from a 'MonadReader' monad.
        , module Imj.Draw.FromMonadReader
        -- * Reexports
        , LayeredColor(..)
        , Coords(..)
        , Alignment(..)
        , Text
        , MonadIO
        , MonadReader
        ) where

import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Text(Text)

import Imj.Draw.Class
import Imj.Draw.FromMonadReader

import Imj.Color(LayeredColor(..))
import Imj.Geo.Discrete(Coords(..))
import Imj.Text.Alignment(Alignment(..))
