# What is it?

A terminal ascii game I write to practice some Haskell. With 12 levels, of increasing difficulty.

In this game, you control a ship in direction, and you can shoot at moving numbers.
During the 5 first seconds, the ship is immune to collisions with numbers, but after that,
if the ship collides with a number it disintegrates.
When the sum of shot numbers is equal to the objective number, the level is completed.

# Backlog

- implement render of space (deduce which wall element based on neighbors)

- flicker : there are several unsynchronized periods:
  - game forward      (fixed start,  fixed period,  low  rate)
  - animation forward (moving start, fixed period,  high rate)
  - user actions      (moving start, moving period, low  rate)

This leads to flicker. We should synchronize more to avoid flushing at very close intervals
(I think this is what produces the flicker).

Prioritize deadlines : animation deadlines have a low priority over game, user actions, messages.

Merge deadlines when they are close to one another, typically both update
the game and some animations in the same timestep (or shoot the laser and animate in the same time step).

To avoid noticeable timing difference when animation deadline is merged, we could even force animations
to happen at fixed start (eg 5 animation steps per game step, and 1 step is common). This has also
the advantage to reduce the need to render when there are multiple animations.

- reconsider which animations to use once gravity based animations are available
- generalize chained sequences on collisions

- random geometry for levels (some numbers might be cycling in separate sub spaces)

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
- Reduce the amount of commands sent to the buffer, so that render goes faster
- use Text instead of String / Data-Text-IO to write strings from Text
- it could be worth calling hPutStr once per frame (using a buffer in RenderState)
- try BlockBuffering (Just 80000)

## Future games
- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
