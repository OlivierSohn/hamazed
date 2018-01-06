- in renderer, add the possibility to just change a color (fg or bg), just change a char.
  - use it to create end level effects : draw everything with '.' or '+' except the winning message
  and the messages on the left.
  - when laser is shot, make a circle expanding, where chars are overridden by '+' or
  color is overridden.
- for wall anims, use same colors as walls, just change the character (Y,X).
- it would be nice to decay the color of animated points when they rebound, until they have no color.
(notion of alpha using back-buffer value + bresenham or "decay color" + bresenham)
to do that , the animation functions need to handle the color of animated points.
- cross-dissolve between worlds, to have smooth transitions (especially important
  when there are walls)

- when frame morphs, it should be centered in the screen, hence:
use an evolution to replace the ship in the center, morph, then use another
evolution to place the ship where it is in the next world.
- rebound animations with decaying velocity when free falling in walls.
we should make the functions field lazy to have an infinite list?
add the notion of velocity to animated points?
-> need to have a TooFar interaction result that stops the growth in that direction even if it's not the last level.
- when animating in a wall, progressively fade with the wall color (notion of alpha)
- do not change background color for outer world animations

- scissors should be combinable : we need a stack of scissors.

- Measure if O2 is necessary, especially for:
  - chained animations with a lot of points
  - delta renderer
compilation times are slower by 4% with -O2.

- for stackage, which ghc versions should I support? https://www.fpcomplete.com/blog/2014/05/stackage-server
