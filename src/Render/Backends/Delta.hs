{-# LANGUAGE NoImplicitPrelude #-}


{-|
= Purpose

Render games or animations in the console, with a strong focus on avoiding
<https://en.wikipedia.org/wiki/Screen_tearing screen tearing>.

= Design goals

* Minimize the amount of data sent to stdout buffer to render a frame.
* Design rendering algorithms that work well for all kinds of game graphics.
* Minimize memory footprint and run-time overhead.

= Features

* <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 8-bit colors> support
* <http://www.unicode.org/ Unicode> support

= Games using it

* <https://github.com/OlivierSohn/hamazed Hamazed>

= A simple example

>
> -- initialization
> ctxt <- createDefaultContext
>
> -- first frame
>
> let black = rgb 0 0 0
>     white = rgb 5 5 5
>     red   = rgb 5 0 0
>
> drawStr "Hello world!" (Coords (Row 10) (Col 20)) (LayeredColor black white) ctxt
> drawStr "How are you?" (Coords (Row 11) (Col 20)) (LayeredColor black white) ctxt
>
> flush ctxt
>
> -- second frame
>
> drawChars '_' 10 (Coords (Row 11) (Col 20)) (LayeredColor black red) ctxt
>
> flush ctxt
>

= History

I'll explain a bit of the context in which I developped this package.

In the beginnings of
<https://github.com/OlivierSohn/hamazed my first ascii based game>,
I implemented a naïve render loop which was:

* At the beginning of the frame, clear the console.
* Render the game elements to stdout.
* At the end of the frame, flush stdout.

The resulting graphics were fluid whilst a frame was ~100 different locations
in a single color. Yet, as color animations were introduced, flickering / tearing
effects started to occur on frames with more rendering locations.

This happened because stdout was saturated by rendering commands and would flush
before all commands corresponding to this frame were issued.

My first move to fix this was to maximize the size of stdout buffer with
<https://hackage.haskell.org/package/base-4.10.1.0/docs/System-IO.html#v:hSetBuffering hSetBuffering>
and
<https://hackage.haskell.org/package/base-4.10.1.0/docs/System-IO.html#v:BlockBuffering BlockBuffering>
parameter:

> hSetBuffering stdout $ BlockBuffering $ Just (maxBound :: Int)

Using 'testStdoutSizes' I measured that my stdout buffer was now 8096 bytes long,
instead of the default 2048 bytes.

This solved the problem, but only until I introduced more animations and more color changes
in the game.

I started thinking that I needed to keep the previous rendered frame and just draw
the difference between the previous and the current frame.

This is exactly what
<https://github.com/ibraimgm Rafael Ibraim> did to solve
<https://github.com/feuerbach/ansi-terminal/issues/5 this issue>.

With <https://gist.github.com/ibraimgm/40e307d70feeb4f117cd his code>,
the render loop becomes:

* Clear the /back/ /buffer/, but not the console!.
* Draw the game elements to the back buffer.
* Render the difference between the front and the back buffer to the console.
* Flush stdout.
* Copy the back buffer to the front buffer.

This approach fixed the rendering of my game, still, I wanted to continue
improving it to cover more use-cases:

* The most obvious optimization I could do was to group the rendered locations
by color before rendering them. On average, one @color change@ command "costs"
20 bytes ("\ESC[48;5;167;38;5;255m"), so grouping them saves a lot of bytes.

* Then, since one @position change@ command "costs" 9 bytes ("\ESC[150;42H") it's
also interesting to optimize for them.
Sorting the "color group" by positions increases the likelyhood that
two consecutive elements are next to one another, and in that case we can omit this
command entirely, because after a 'putChar' the cursor location goes one step to the
right. This also saved some bytes.

* Also
<https://www.reddit.com/r/haskellquestions/comments/7i6hi5/optimizing_memory_usage_array_of_unboxed_values/
I found usefull answers on reddit> which helped in the process of optimizing
the memory layout. Contiguous memory blocks are mow used to store information
on rendered locations, and we use an Unboxed 'Word64', which is the most efficient Haskell type in terms of
"information qty / memory usage" ratio, to encode (from
higher bits to lower bits):

    * background color  (Word8)
    * foreground color  (Word8)
    * buffer position   (Word16)
    * unicode character (Word32)

There will be more optimizations, and I'll try to keep them general enough so
that they benefit every use case.
-}
module Render.Backends.Delta
                          ( -- * Create context, set policies
                            module Render.Backends.Internal.Buffers
                            -- * Draw
                          , module Render.Backends.Internal.Draw
                            -- * Render to the console
                          , module Render.Backends.Internal.Flush
                          , BufferMode(..)
                          , preferredBuffering
                          ) where

import           Imajuscule.Prelude

import           System.IO( BufferMode(..) )

import           Render.Backends.Internal.Buffers
import           Render.Backends.Internal.Draw
import           Render.Backends.Internal.Flush

preferredBuffering :: BufferMode
preferredBuffering = BlockBuffering $ Just (maxBound :: Int)
