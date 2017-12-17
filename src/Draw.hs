{- | These modules provide a way to abstract a global renderer, in a style adhering to
<https://www.fpcomplete.com/blog/2017/06/readert-design-pattern these recommendations>
regarding global state in a program.

* As a user of these modules, you embed the 'IORef' 'Buffers' in 'Env' (your program environment)
and write a 'Draw' instance for 'Env'.

    * <https://github.com/OlivierSohn/hamazed/blob/master/src/Env.hs This example>
    based on renderer "Render.Delta" shows how to do it.

* Then, you can write:

@
helloWorld :: (Draw e, MonadReader e m, MonadIO m)
           => m ()
helloWorld = do
  drawTxt \"Hello\" (Coords 10 10) red
  drawTxt \"World\" (Coords 20 20) green
  renderDrawing

main = do
  env <- createEnv
  runReaderT helloWorld env
@
-}

module Draw
        ( module Draw.Class
        , module Draw.ReaderHelpers
        )
        where

import Draw.Class
import Draw.ReaderHelpers
