{- | These modules provide a way to abstract a global renderer, in a style adhering to
<https://www.fpcomplete.com/blog/2017/06/readert-design-pattern these recommendations>
regarding global state in a program.

* As a user of these modules, you embed the drawing functions in your "environment" 'Env'
by writing a 'Draw' instance for 'Env'.
* Then, using a 'ReaderT' 'Env'
monad transformer, and the helper functions defined in "Draw.Helpers" or "Draw.Aligned" you can write:

@
helloWorld :: (Draw e) => ReaderT e IO ()
helloWorld = do
  drawTxt \"Hello\" (Coords 10 10) red
  drawTxt \"World\" (Coords 20 20) green
  flush

main = do
  env <- createEnv
  runReaderT helloWorld env
@

<https://github.com/OlivierSohn/hamazed/blob/master/src/Env.hs This example>
based on renderer "Render.Delta" shows implementations of 'createEnv' and the
environment's 'Draw' instance.
-}

module Draw
        ( module Draw.Class
        , module Draw.Helpers
        , module Draw.Aligned
        )
        where

import Draw.Class
import Draw.Helpers
import Draw.Aligned
