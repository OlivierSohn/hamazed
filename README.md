# What is it?

A terminal ascii game I write to practice some Haskell! It has 12 levels, and I reached level 9 twice!

# Backlog

## Animation Design
- when first explosion element hits the world limits, a second explosion occurs using a circle
- take number speed & laser direction into account for animation
- make gravity based animations : initial velocity, rebound on walls, lose % velocity when hitting the bottom
- combine animation types : explode animation converts to gravity based upon hitting the boundaries

## Game Fluidity
- when lose, do not stop animations, stop only move.
- make laser ray render persist until next motion step.
- do not count duplicate laser shots in same motion step.

## Visibility
- make target more visible (color?)

- write a help :
 -- The goal of the game is to hit numbers to reach a target. When a number is hit with a laser ray (i j k l) it is added to the current sum. The target is exactly the sum of all numbers divided by 2.
During the 5 first seconds, the ship is immune to collisions, drawn in red, and then turns blue. If the ship collides
with a number the game is over.

## Difficulty
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
