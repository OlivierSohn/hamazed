{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE NoImplicitPrelude #-}

{-|
The purpose of this module is to render games and animations in the terminal
without <https://en.wikipedia.org/wiki/Screen_tearing screen tearing>.

It supports <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 8-bit Colors>
and <http://www.unicode.org/ Unicode> characters.

In short, <https://en.wikipedia.org/wiki/Screen_tearing screen tearing>
is mitigated by:

    * Using double buffering techniques (/back/ and /front/ buffers)
    * Rendering in each frame /only/ the locations that have changed, in an order
    that allows to omit many byte-expensive commands,
    * Chosing the smallest rendering command among equivalent alternatives.

A more detailed overview can be seen at the end of this documentation.
-}

module Imj.Graphics.Render.Delta
  ( -- * Usage
{- |
* from a 'MonadIO' monad:

    @
    import Imj.Graphics.Class.Draw(drawStr')
    import Imj.Graphics.Class.Render(renderToScreen')

    helloWorld :: (MonadIO m) => m ()
    helloWorld env = do
      drawStr' env \"Hello World\" (Coords 10 10) green
      renderToScreen' env

    main = runThenRestoreConsoleSettings $ newDefaultEnv >>= helloWorld
    @

* from a 'MonadIO', 'MonadReader' 'DeltaEnv' monad,

    @
    import Imj.Graphics.Render.FromMonadReader(drawStr, renderToScreen)

    helloWorld :: (Draw e, Render e, MonadReader e m, MonadIO m) => m ()
    helloWorld = do
      drawStr \"Hello World\" (Coords 10 10) green
      renderToScreen

    main = runThenRestoreConsoleSettings $ newDefaultEnv >>= runReaderT helloWorld
    @

* from a 'MonadIO', 'MonadReader' 'YourEnv' monad,

    * assuming 'YourEnv' owns a 'DeltaEnv'
    and implements a 'Draw' instance which forwards to the 'Draw' instance of
    the 'DeltaEnv' (like in
    <https://github.com/OlivierSohn/hamazed/blob/master/imj-game-hamazed/src/Imj/Game/Hamazed/Env.hs this game>):

    @
    import YourApp(createYourEnv)
    import Imj.Graphics.Render.FromMonadReader(drawStr, renderToScreen)

    helloWorld :: (Draw e, Render e, MonadReader e m, MonadIO m) => m ()
    helloWorld = do
      drawStr \"Hello World\" (Coords 10 10) r green
      renderToScreen

    main = runThenRestoreConsoleSettings $ newDefaultEnv >>= createYourEnv >>= runReaderT helloWorld
    @
-}

  -- * Environment
  -- | Back and front buffers are persisted in the delta-rendering environment:
  -- 'DeltaEnv'.
  DeltaEnv
  -- ** Environment creation
, newDefaultEnv
, newEnv
-- ** Policies
{- | Note that policy changes take effect after the next render. -}
-- *** Resize
, ResizePolicy(..)
, defaultResizePolicy
, setResizePolicy
-- *** Clear after render
, ClearPolicy(..)
, defaultClearPolicy
, setClearPolicy
, defaultClearColor
, setClearColor
-- ** Stdout BufferMode
{- When using 'setStdoutBufferMode', the stdout 'BufferMode' change is applied
immediately. -}
, defaultStdoutMode
, setStdoutBufferMode
  -- * Draw and render
  {- | The functions below present drawing and rendering functions in a 'MonadReader'
  monad, which is the recommended way to use delta rendering.

  More alternatives are presented in this module:
  -}
, module Imj.Graphics.Render
, module Imj.Graphics.Render.FromMonadReader
  -- * Cleanup
, module Imj.Graphics.Render.Delta.Console
-- * Reexports
, BufferMode(..)

-- * Motivations and technical overview
{- |

= Screen tearing

<https://en.wikipedia.org/wiki/Screen_tearing Screen tearing> occurs in the terminal
when, for a given frame, rendering commands exceed the capacity of stdout buffer.
To avoid overflowing stdout, the system flushes it, thereby triggering a /partial/ frame render.

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

=== Minimizing the total size of rendering commands

The initial implementation was fixing the screen tearing for my game, yet I wanted
to optimize things to be able to support even richer frame changes in the future.
I added the following optimizations:

* We group locations by color before rendering them, to issue one @color change@
per group instead of one per element (an 8-bit @color change@ command is 20 bytes:
@"\ESC[48;5;167;38;5;255m"@).

* We render the "color group" elements by increasing screen positions, and when two
consecutive elements are found, we omit the @position change@ command,
because 'putChar' already moved the cursor position to the right (a 2-D
@position change@ command is 9 bytes: @"\ESC[150;42H"@).

We can still improve on this by using a one-dimensional
@relative position change@ commands (3 to 6 bytes : @"\ESC[C"@, @"\ESC[183C"@)
when the next location is either on the same column or on the same line.

=== Minimizing the run-time overhead and memory footprint

I wanted not only to avoid screen tearing, but also to be fast, to allow for higher
framerates. So I refactored the datastructures to use continuous blocks of memory,
and to encode every important information in the smallest form possible, to improve cache usage.

<https://www.reddit.com/r/haskellquestions/comments/7i6hi5/optimizing_memory_usage_array_of_unboxed_values/ These answers on reddit>
helped in the process.

I use Vectors of unpacked 'Word64' (<https://wiki.haskell.org/GHC/Memory_Footprint the most efficient Haskell type in terms of "information quantity / memory usage" ratio>)
and an efficient encoding to stores 4 different informations in a Word64:

/[from higher bits to lower bits]/

    * background color  (8 bits)
    * foreground color  (8 bits)
    * buffer position   (16 bits)
    * unicode character (32 bits)

I also introduced a third in-memory vector, the "Delta" vector, which contains just the differences to render.
Due to the previously described encoding, when <http://hackage.haskell.org/package/vector-algorithms-0.7.0.1/docs/Data-Vector-Algorithms-Intro.html sorting>
the delta vector, same-color locations end up being grouped in the same slice of the vector,
and are sorted by increasing position, which is exactly what we want to implement the optimizations I mentionned earlier.
-}
  ) where

-- TODO add a section on 'Performance documentation' to report on the amount of bytes
-- sent to stdout with concrete examples.

import           Imj.Graphics.Render
import           Imj.Graphics.Render.Delta.Env
import           Imj.Graphics.Render.Delta.Console
import           Imj.Graphics.Render.FromMonadReader
