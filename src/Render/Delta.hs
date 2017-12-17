{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE NoImplicitPrelude #-}

{-|
= Introduction

<https://en.wikipedia.org/wiki/Screen_tearing Screen tearing> occurs in the terminal
when, for a given frame, rendering commands exceed the capacity of stdout buffer.
To avoid overflowing stdout, the system flushes it, thereby triggering a /partial/ frame render.

During a game, occasional partial frames distract the player, hence, it is crucially
important to address this issue. And this is exactly what this package is about!

If you encounter screen tearing issues in you game, and if this package doesn't
the issue entirely, please tell me, I'll investigate :-)

= API usage

Due to the nature of the rendering algorithms, we need /back/ and /front/
buffers to be persisted inside a 'DeltaEnv' environment.

This environment is created once ('newDefaultEnv', 'newEnv') and reused
throughout the program.

With it, you will draw colored elements and render to the console:

* From a 'MonadIO' monad :

    @
    import Draw.Class(drawStr', renderDrawing')

    main = newDefaultEnv >>= helloWorld

    helloWorld :: (MonadIO m) => m ()
    helloWorld env = do
      drawStr' env \"Hello\" (Coords 10 10) red
      drawStr' env \"World\" (Coords 20 20) green
      renderDrawing' env
    @

* From a 'MonadReader' monad,

    * on a 'DeltaEnv' :

    @
    import Draw.Helpers.MonadReader(drawStr, renderDrawing)

    main = newDefaultEnv >>= runReaderT helloWorld

    helloWorld :: (Draw e, MonadReader e m, MonadIO m) => m ()
    helloWorld = do
      drawStr \"Hello\" (Coords 10 10) red
      drawStr \"World\" (Coords 20 20) green
      renderDrawing
    @

    * or on your own environment:

        * You will need to add a field to your environment to store the 'DeltaEnv',
        and implement a 'Draw' instance for your environment, which will use
        the 'Draw' instance of 'DeltaEnv'.
        * You can refer to
        <https://github.com/OlivierSohn/hamazed/blob/master/src/Env.hs this example>
        to see an implementation following this pattern.

    @
    import Draw.Helpers.MonadReader(drawStr, renderDrawing)
    import MyApp(createMyEnv)

    main = newDefaultEnv >>= createMyEnv >>= runReaderT helloWorld

    helloWorld :: (Draw e, MonadReader e m, MonadIO m) => m ()
    helloWorld = do
      drawStr \"Hello\" (Coords 10 10) red
      drawStr \"World\" (Coords 20 20) green
      renderDrawing
    @

= Features

The functions exported by this module allow to render complex animated graphics.

<https://en.wikipedia.org/wiki/Screen_tearing Screen tearing> is mitigated by:

* Using double buffering techniques
* Rendering in each frame /only/ the locations that have changed, in a smart order
that allows to omit many byte-expensive commands,
* Chosing the smallest (in bytes) rendering command when there are equivalent alternatives.

<https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 8-bit Colors>
are supported, as well as <http://www.unicode.org/ Unicode>
characters.

A particular attention is given to using algorithms and datastructures that
achieve a low run-time overhead and a reduced memory footprint (see below).

= Motivations

At the beginning of the development of
<https://github.com/OlivierSohn/hamazed hamazed, my first terminal game>,
I was clearing the screen at every frame and filling stdout with rendering commands
for every game element and animation.

As the complexity of animations grew, screen tearing occured, so I looked for ways to fix it.
This package is the result of this research.

My first idea to mitigate screen tearing was to maximize the size of stdout buffer:

> hSetBuffering stdout $ BlockBuffering $ Just maxBound

Using 'testStdoutSizes' I measured that it went from 2048 bytes to 8096 bytes long.
But it solved the problem only very temporarily. As I introduced more animations in the game,
screen tearing was back!

I needed to not only maximize stdout size but also reduce the amount of data that
I was writing in it.

== Delta rendering

Delta rendering is the approach
<https://github.com/ibraimgm Rafael Ibraim> took when
<https://gist.github.com/ibraimgm/40e307d70feeb4f117cd writing this code> for his own game.

The idea was to introduce two in-memory buffers:

* a /front/ buffer containing what is currently displayed on the terminal
* a /back/ buffer containing what we want to draw in the next frame.

At every frame, we would draw all game elements and animations,
this time /not/ to the terminal directly, but to the back buffer.

At the the end of the frame, the difference between front and back buffer would
be rendered to the terminal.

== Further optimizations

=== Minimizing the total size of rendering connamds

The initial implementation was fixing the screen tearing for my game, yet I wanted
to optimize things to be able to support even richer frame changes in the future.
I added the following optimizations:

* We group locations by color before rendering them, to issue one @color change@
per group instead of one per element (an 8-bit @color change@ command is 20 bytes:
"\ESC[48;5;167;38;5;255m").

* We render the "color group" elements by increasing screen positions, and when two
consecutive elements are found, we omit the @position change@ command,
because 'putChar' already moved the cursor position to the right (a 2-D
@position change@ command is 9 bytes: "\ESC[150;42H").

We can still improve on this by using a one-dimensional
@relative position change@ commands (3 to 6 bytes : "\ESC[C", "\ESC[183C")
when the next location is either on the same column or on the same line.

=== Minimizing the run-time overhead and memory footprint

I wanted not only to avoid screen tearing, but also to be fast, to allow for higher
framerates. So I refactored the datastructures to use continuous blocks of memory,
and to encode every important information in the smallest form possible, to improve cache usage.

<https://www.reddit.com/r/haskellquestions/comments/7i6hi5/optimizing_memory_usage_array_of_unboxed_values/ These answers on reddit>
helped in the process.

I ended up using Vectors of unpacked 'Word64' (<https://wiki.haskell.org/GHC/Memory_Footprint the most efficient Haskell type in terms of "information quantity / memory usage" ratio>)
where 4 kinds of informations were encoded in a single Word64:

    * [from higher bits to lower bits]
    * background color  (8 bits)
    * foreground color  (8 bits)
    * buffer position   (16 bits)
    * unicode character (32 bits)

I also introduced a third in-memory vector, the "Delta" vector, which contains just the differences to render.
Due to the previously described encoding, when <http://hackage.haskell.org/package/vector-algorithms-0.7.0.1/docs/Data-Vector-Algorithms-Intro.html sorting>
the delta vector, same-color locations end up being grouped in the same slice of the vector,
and are sorted by increasing position, which is exactly what we want to implement the optimizations I mentionned earlier.

= Performance documentation

Here I'll report on the amount of bytes sent to stdout with concrete examples.

-}

module Render.Delta
          ( -- * Environment creation / config
            module Render.Delta.Env
            -- * Draw and render:
            -- ** - From a MonadReader monad
          , module Draw.Helpers.MonadReader
            -- ** - From a MonadIO monad
          , module Draw.Class
          ) where

import           Draw.Class
import           Draw.Helpers.MonadReader
import           Render.Delta.Env
