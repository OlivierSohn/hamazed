
# Backlog

- make sure the ship is not at the place of a number at the beginning

- let users chose:
 - shape of the world
  - square
  - 2x1 rectangle
 - animation types
 - walls
  - none
  - deterministic
  - random
   - blocksize
   - strategy
    - one cc
    - biggest cc

- it seems that the console has a fixed refresh rate of 21 fps, so if we render an a slightly different fps
there will be every once in a while a frame than will be slow (as if a frame was skipped) for animations.
It would be nice to synchronize animation exactly with console fps to have a better fluidity

- http://dev.stephendiehl.com/hask/#what-to-avoid
- http://www.stephendiehl.com/posts/protolude.html

- fancy laser animation: at each step, one less point or they could fall down with gravity + random, and fade in intensity

- reconsider which animations to use once gravity based animations are available
- generalize chained sequences on collisions

## Random world constraints

- take the biggest connected component (in terms of area? number of cells? both?) and put
numbers and ship in it. The other air connected components can be converted to walls.
This relaxes the constraint on having a single connected component.

- prevent "reduced rectangle" effect (i.e. make sure on each side there is Air touching the border)

## Event driven aspect
The game is driven by these events:

|evt name         |start of the period|period length|rate|number of instances at any given time|
|-----------------|-------------------|-------------|----|-------------------------------------|
|game forward     |start of game      |constant     |med |1                                    |
|animation forward|anytime            |constant     |high|0..n                                 |
|user event       |anytime            |not periodic |low |0..1                                 |

- As of today, if an animation takes a long time to compute (more that its period), it will slow down
the game because there is no notion of priority. We could implement following priorities:
game forward > user events > animation forward. The algorithm to find the deadline to address in the
next loop is then :
  - traversing event types by decreasing priorities, find the first overdue deadline.
  - If an overdue deadline was found, address in the next game loop, else address
the closest deadline.

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
- when 2 rooms are separated by a wall, the wall can be destroyed by
hitting a key (the key should be present also in the other room)
- or each room has an objective number, once the objective is reached, a door opens to the next room

## Graphics
- make an animation between levels to make the world reduce progressively

## Animation Design
- take number speed & laser direction into account for animation
- make gravity based animations : initial velocity, rebound on walls, lose % velocity when hitting the bottom
- combine animation types : explode animation converts to gravity based upon hitting the boundaries

## Playability
- make laser ray render persist until next motion step.
- do not count duplicate laser shots in same motion step.
- write a help

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
- when shooting the laser, we could render less to optimize lag
- try BlockBuffering (Just 80000)

## Future games
- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
