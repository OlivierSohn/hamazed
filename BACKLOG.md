- separate outside world from inside, at the frame position.
- when the animation does this:
  mutate with precollision on point A, one animated point only
   mutate with precollision on point A, one animated point only
    mutate with precollision on point A, one animated point only
  if the animations functions are "one function repeating",
  it is equivalent to not mutate, and instead change the center of the 'AnimatedPoints'.
  it will save some CPU and RAM.
- it would be nice to decay the color of animated points when they rebound, until they have no color.
(notion of alpha using backbuffer value + bresenham or "decay color" + bresenham)
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
