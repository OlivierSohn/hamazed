
# Backlog

## Misc.

- use hspec for testing

- provide "nice" default colors (notion of palette?)

- some evolutions don't use the timing aspect as another evolution times them.
some evolutions don't use the interpolation aspect as they are only used to time.
we should separate the 2 aspects by defining 2 distinct types.

## Geometry

- take into account the fact that character width and height are not equal,
so geometric figures are stretched in height. We could compensate for that by using
a stretching factor in poly function and circle function

## Animation Design

- Today the contract of pure animation functions is that they should return
a constant number of animation points that are correlated across frames.
We could also make them say "After frame x, since my animation is done
I will return an empty list" : it could allow to stop animations that have
"CanInteract DontInteract" and will continue indefinitely
or to interpret "CanInteract DontInteract" as "CanInteract Interact" for animations that don't guarantee they will end

- generalize chained sequences on collisions
  - try passing a list of functions to the tree's 'treeOnWall' Rebound
- when an animation point touches the world frame, make it change color

## Playability
- it seems that the console has a fixed refresh rate of 21 fps, so if we render an a slightly different fps
there will be every once in a while a frame than will be slow (as if a frame was skipped) for animations.
It would be nice to synchronize animation exactly with console fps to have a better fluidity

## Games to implement:
- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
