{-# LANGUAGE NoImplicitPrelude #-}

{- |
= Foreword

<https://github.com/OlivierSohn/hamazed/blob/master/imj-game-hamazed/src/Env.hs This concrete example>
shows how I use 'Draw' in a game to use the renderer via a 'ReaderT' monad.

= Description

These modules provide a way to abstract a global renderer, in a style adhering to
<https://www.fpcomplete.com/blog/2017/06/readert-design-pattern these recommendations>
regarding global state in a program.

* As a user of these modules, you will run your program in a 'MonadReader' 'Env' monad,
where 'Env' is your environment and it has a 'Draw' instance.
* Then, you can simply write:

@
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Reader.Class(MonadReader)
import Control.Monad.Reader(runReaderT)

import Draw.Class
import Draw.Helpers.MonadReader(drawTxt, renderDrawing)

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

-}

module Draw
        ( module Draw.Class
        , module Draw.Helpers.MonadReader
        , module Draw.Aligned
        , module Draw.ColorString
        ) where

import Draw.Aligned
import Draw.Class
import Draw.ColorString
import Draw.Helpers.MonadReader
