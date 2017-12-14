{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE NoImplicitPrelude #-}

{-|
= Introduction

<https://en.wikipedia.org/wiki/Screen_tearing Screen tearing> occurs in the console
when, for a given frame, rendering commands exceed the capacity of stdout buffer.
To avoid overflow, the system flushes stdout, thereby triggering a /partial/ frame render.

During a game, occasional partial frames distract the player. Hence, it is crucially
important to address this issue! If you encounter screen tearing issues in you game,
give this package a try.

= Usage

> -- Example : Hello World
>
> import Render.Delta
>
> main = createDefaultContext  -- call this once per program, then reuse the context
>          >>= drawStr "Hello World" (Coords 11 20) (LayeredColor black white)
>            >>= flush  -- call this once per frame, to render it to the console

= Technical highlights

The functions exported by this module allow to render complex animated graphics and mitigate
<https://en.wikipedia.org/wiki/Screen_tearing screen tearing> by

* Using double buffering techniques
* Rendering in each frame /only/ the locations that have changed, in a smart order
that allows to omit many byte-expensive commands,
* Chosing the smallest (in bytes) rendering command when there are equivalent alternatives.

<https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 8-bit colors>
are supported (216 RGB and 24 Grays), as well as <http://www.unicode.org/ Unicode>
characters.

A particular attention is given to using algorithms and datastructures that
achieve a low run-time overhead and a reduced memory footprint (see below).

= Motivations

At the beginning of the development of
<https://github.com/OlivierSohn/hamazed hamazed, my first console game>, at every frame
I was clearing the screen and filling stdout with rendering commands for every game element and animation.

Screen tearing started to occur as I increased the complexity number of animations.
So I maximized the size of stdout buffer:

> hSetBuffering stdout $ BlockBuffering $ Just (maxBound :: Int)

Using 'testStdoutSizes' I measured that it went from 2048 bytes to 8096 bytes long.
It solved the problem only until I introduced more animations in the game!

stdout was already at its maximum size so I had to reduce the amount of data that I was throwing at it.

== Delta rendering

The idea was to introduce two in-memory buffers:

* a /front/ buffer containing what is currently displayed on the console
* a /back/ buffer containing what we want to draw in the next frame.

At every frame, I would draw all game elements and animations,
this time /not/ to the console directly, but to the back buffer.

Then, at the end of the frame, the difference between front and back buffer would
be rendered to the console.

This is the approach
<https://github.com/ibraimgm Rafael Ibraim> took when
<https://gist.github.com/ibraimgm/40e307d70feeb4f117cd writing this code> for his own game.
I used his code and it fixed screen tearing for my game.

== Further optimizations

=== Minimizing total size of rendering connamds

I further improved the code to support richer frame changes:

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

=== Minimizing run-time overhead and memory footprint

<https://www.reddit.com/r/haskellquestions/comments/7i6hi5/optimizing_memory_usage_array_of_unboxed_values/ These answers on reddit>
helped in the process: location informations are now stored in a continuous block of memory,
using an unpacked 'Word64' (<https://wiki.haskell.org/GHC/Memory_Footprint the most efficient Haskell type in terms of "information quantity / memory usage" ratio>)
to encode location information (from higher bits to lower bits):

    * background color  (8 bits)
    * foreground color  (8 bits)
    * buffer position   (16 bits)
    * unicode character (32 bits)

A third in-memory vector, the "Delta" vector, contains just the differences to render, and
using ther previously described encoding, when <http://hackage.haskell.org/package/vector-algorithms-0.7.0.1/docs/Data-Vector-Algorithms-Intro.html sorting>
it, same-color locations are grouped in the same slice of the vector, and sorted by increasing position,
which is convenient to implement the optimizations I mentionned earlier.

= Performance documentation

Here I'll report on the amount of bytes sent to stdout with concrete examples.

-}
module Render.Backends.Delta
                          ( module Render.Backends.Internal.Buffers
                            -- * Frame
                            -- ** Draw
                          , module Render.Backends.Internal.Draw
                            -- ** Render
                          , module Render.Backends.Internal.Flush
                          -- * Types
                          , module Render.Types
                          , BufferMode(..)
                          , preferredBuffering
                          ) where

import           Imajuscule.Prelude

import           System.IO( BufferMode(..) )

import           Render.Backends.Internal.Buffers
import           Render.Backends.Internal.Draw
import           Render.Backends.Internal.Flush
import           Render.Types

preferredBuffering :: BufferMode
preferredBuffering = BlockBuffering $ Just (maxBound :: Int)
