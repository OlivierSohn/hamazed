
# Backlog

- for terminal, allow the points to go a little further up so that gravity makes them come back
when shooting upwards

## Geometry

- take into account the fact that character width and height are not equal,
so geometric figures are stretched in height. We could compensate for that by using
a stretching factor in poly function and circle function

## Packaging

- make a package for Delta rendering:
  - review function names
  - create repo, test locally using https://stackoverflow.com/questions/32849269/how-to-install-use-a-local-version-of-package-using-stack
  - CI: https://github.com/hvr/multi-ghc-travis

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

- Flicker can happen if we flush at very close intervals. This behaviour has been greatly improved
by using delta rendering, however, I don't think that it fixes it entirely. So there are a few
things we could do to help keeping a minimal time between flush intervals while keeping a good
game aspect:
  - Handle deadlines that are close to one another using the same rendering frame (typically update
the game and animate in the same frame, or shoot the laser and animate).
  - To avoid noticeable timing difference when animation deadline is merged, we could even force animations
to happen at fixed start (eg 5 animation steps per game step, and 1 step is common). This has also
the advantage to reduce the need to render when there are multiple animations.

## Game Notions
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

- have several lists of animations
  - each list has a different collision function:
    - world
    - world (inner walls only) + terminal
    - terminal only
  - or keep one list but animation passes a parameter to function to say which check to do
     ... how could this parameter depend on the sequence of the animation? could it be preapplied?
  - applications :
    - final explosion happends in the terminal.
      - try explosionThenGravity or gravityExplosionThenSimpleExplosion
    - for gravity-based animations we could allow to go through the edge of the world (but not through other walls)
- generalize chained sequences on collisions
  - try passing a list of functions to the tree's 'treeOnWall' Rebound
- make an animation between levels to make the world reduce progressively
- use bresenham 3d for smooth color gradients (code commented in Geo)
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
- try BlockBuffering (Just 80000)

## Windows support

- The keyboard input is not flushed until we press enter: it is a known bug
    https://ghc.haskell.org/trac/ghc/ticket/2189
  - potential workaround :
  before calling `getChar`, call `hIsReady stdin` : if it returns true,
    send a newline as input then call `getChar` (use https://stackoverflow.com/questions/19578565/simulating-keystrokes-with-haskell-on-windows)

## Future games
- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
