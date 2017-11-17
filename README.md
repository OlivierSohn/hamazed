# What is it?

A terminal ascii game I write to practice some Haskell. It has 12 levels of increasing difficulty.

In this game, you control a ship in direction, and you can shoot at moving numbers.
During the 5 first seconds, the ship is immune to collisions with numbers, but after that,
if the ship collides with a number it disintegrates.
When the sum of shot numbers is equal to the objective number, the level is completed.

Keyboard Controls:
- for ship : sedf
- for laser: jikl

Version history:
- 1.0 : The world is a square.

# Credits

Rafael Ibraim published https://gist.github.com/ibraimgm/40e307d70feeb4f117cd which is
delta rendering in the console. It avoids to have to clear the console and redraw
everything at each frame, instead only the parts of the console that have changed are
rendered. It removes unwanted flickering effects.

# Backlog

- find a way to make laser shots visible to the user even when they happen at the very
end of a game step:
  - after a laser shot we could guarantee a minimal time before the next render
  - or we could draw it for several seconds (then we need a timeout to remove it)

- use Text instead of String in rendered world

- when ship goes too fast it is possible to go through walls ! fix it...

- implement render of space (deduce which wall element based on neighbors)

- reconsider which animations to use once gravity based animations are available
- generalize chained sequences on collisions

- random geometry for levels (some numbers might be cycling in separate sub spaces)

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
