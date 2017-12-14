
# Backlog

- generic use of bresenham for interpolations of 2 and 3 dimensions

## Delta rendering

- dependencies:

make a package with
  - geometric figures / colored text
   -> /Geo
  - color
  - interpolable
  - interpolated colors, interpolated text
  - animations
  - Collision.hs
  - Timing
  - Iteration
  - Math
  - aligned rendering

- make a package for Delta rendering:
  - create repo, test locally using https://stackoverflow.com/questions/32849269/how-to-install-use-a-local-version-of-package-using-stack
  - CI: https://github.com/hvr/multi-ghc-travis

- make tests to see the effect of buffer size on screen tearing :
  - alternate '|' with '-'
  - same with random colors
  - same with worst case : background and foreground change all the time,
        same-background colors are far away (to avoid positional optimization)
- create an app to test engine.

- Another optimization is to chose the type of "position change"
command we send based on the relative location of successive elements: today
we use exclusively the 2 dimensional version (9 bytes on average), but it would
be more efficient to switch to
  - the "go forward (optionally n times)" version (3 to 5 bytes) when the
  previously rendered element was on the same row.
  - the "printStrLn "

- measure if it's faster to buffer the string sent to putStr on our side

- stats to see stdout buffer usage

- test Unicode support (document, provide an example)

## Misc.

- 3..2..1..GO! countdown at the beginning of a level

- animate lose/win messages

- when terminal is resized, re-center world accordingly.
- if terminal size is too small, pause the game and display a message

- make inter-level animation time independent of compute / render time
-> keep "the last time world evolution was rendered" in worldEvolution to not make deadlines based on current time

- some evolutions don't use the timing aspect as another evolution times them.
some evolutions don't use the interpolation aspect as they are only used to time.
we should separate the 2 aspects by defining 2 distinct types.

- animate frame color while transitionning (red -> white -> red)

- when expanding, invert animation order between frame and infos

- use a dedicated animation for world swap, and then one for game start

- verify timing for ship safety when transitioning, currently it starts from beginning of animation!

- reduction from 2 edges

- for world with rectangular shape, the walls become visible at the other side of the fronteer
on animations. Maybe we should animate the world too.

At least do a rendering mask.

- a mode with limited visibility (a circle around the ship)

- animate colors of "You win / You lost"
using colors of numbers for win / colors of explosions for lost

- for terminal, allow the points to go a little further up so that gravity makes them come back
when shooting upwards

## Geometry

- take into account the fact that character width and height are not equal,
so geometric figures are stretched in height. We could compensate for that by using
a stretching factor in poly function and circle function

## Configurability

- let users chose:
  - blocksize

## Random world constraints

- allow multiple connected components, but use the biggest connected component
(in terms of area? number of cells? both?) and put numbers and ship in it.
The other air connected components could be converted to walls.
This relaxes the constraint on having a single connected component.

- prevent "reduced rectangle" effect (i.e. make sure on each side there is Air touching the border)

## Event driven aspect
The game is driven by these events:

|event            |start of the period|period length|rate|number of instances at any given time|priority|
|-----------------|-------------------|-------------|----|-------------------------------------|--------|
|game forward     |start of game      |constant     |med |1                                    |1       |
|animation forward|anytime            |constant     |high|0..n                                 |3       |
|user event       |anytime            |not periodic |low |0..1                                 |2       |

## Game Notions
- some numbers are hidden in the walls
- 3 lives at the beginning, gain one life if you complete a level in less than 5 seconds.
- walls are destroyable and contain ammo
- frame walls can be destroyed in 2 laser shots
- when 2 rooms are separated by a wall, the wall can be destroyed by using the laser.
hitting a key (the key should be present also in the other room)
- or each room has an objective number, once the objective is reached, a door opens to the next room

## Animation Design

- Today the contract of pure animation functions is that they should return
a constant number of animation points that are correlated across frames.
We could also make them say "After frame x, since my animation is done
I will return an empty list" : it could allow to stop animations that have
"OnWall Traverse" and will continue indefinitely
or to interpret "OnWall Traverse" as "OnWall ReboundAnd" for animations that don't guarantee they will end

- generalize chained sequences on collisions
  - try passing a list of functions to the tree's 'treeOnWall' Rebound
- make an animation between levels to make the world reduce progressively
  for this animation we need to render the frame on top of the world

  it's more complicated than an animation because there are multiple chars
- when an animation point touches the world frame, make it change color
- use a different animation when the target is met?

## Playability
- do not count duplicate laser shots in same motion step.
- write a help
- it seems that the console has a fixed refresh rate of 21 fps, so if we render an a slightly different fps
there will be every once in a while a frame than will be slow (as if a frame was skipped) for animations.
It would be nice to synchronize animation exactly with console fps to have a better fluidity

## Difficulty
- choosing different prime numbers for width and height would increase the complexity
because the trajectories would "change all the time". Right now width == height so it's easy to chose
a spot where no number will ever be.
- current implementation is easy mode, make a medium mode where the
numbers of lasers available is different for each direction
and a hard mode where there is a timeout on each level.
An easier mode would be to have the ship be totally immune to collisions.

- Change the motion period for various difficulty levels

## Rendering optimizations
- it could be more efficient to have a global contiguous buffer for the string that will be actually written.

## Future games
- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
