{- | These modules provide a way to abstract a global renderer, in a style adhering to
<https://www.fpcomplete.com/blog/2017/06/readert-design-pattern these recommendations>
regarding global state in a program.

The idea is the following: you embed the drawing functions in your "environment" 'Env'
by writing a 'Draw' instance for 'Env'.

Then, using a 'ReaderT' 'Env'
monad transformer, and the helper functions defined in "Render.Helpers" or "Render.Aligned" you can write:

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

In its file 'src/Env.hs',
<https://github.com/OlivierSohn/hamazed this game>
shows an example implementation
of 'Env', 'createEnv', and the 'Draw' instance of 'Env',
-}

module Render
        ( module Render.Draw
        , module Render.Helpers
        , module Render.Aligned
        )
        where

import Render.Draw
import Render.Helpers
import Render.Aligned
