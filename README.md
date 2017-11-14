# What is it?

A terminal ascii game I write to practice some Haskell. With 12 levels, of increasing difficulty.

# Backlog

## Animation Design
- take number speed & laser direction into account for animation
- make gravity based animations : initial velocity, rebound on walls, lose % velocity when hitting the bottom
- combine animation types : explode animation converts to gravity based upon hitting the boundaries

## Playability
- make laser ray render persist until next motion step.
- do not count duplicate laser shots in same motion step.
- at the top, display
                   8 (red)
           3+1+4 = 8
- display level at the bottom (4/12), centered
- write a help :
 - The goal of the game is to hit numbers to reach a target. When a number is hit with a laser ray (i j k l) it is added to the current sum. The target is exactly the sum of all numbers divided by 2.
During the 5 first seconds, the ship is immune to collisions, drawn in red, and then turns blue. If the ship collides
with a number the game is over.

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
