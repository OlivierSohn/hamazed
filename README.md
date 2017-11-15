# What is it?

A terminal ascii game I write to practice some Haskell. With 12 levels, of increasing difficulty.

In this game, you control a ship in direction, and you can shoot at moving numbers.
During the 5 first seconds, the ship is immune to collisions with numbers, but after that,
if the ship collides with a number it disintegrates.
When the sum of shot numbers is equal to the objective number, the level is completed.

# Backlog

use the simpleExplosionUntilCollisions as 2nd element of the hardcoded sequence
this will show how to generalize chained sequences on collisions

implement, then genericize with "1whileInsideWorldThen2"

- implement render of space (deduce which wall element based on neighbors)
- random geometry for levels (some numbers might be cycling in separate sub spaces)

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
- it could be worth calling hPutStr once per frame (using a buffer in RenderState)
- try BlockBuffering (Just 80000) to reduce flicker

## Future games
- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
