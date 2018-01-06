
# Backlog

## Misc.

- do not render 2 times within 5 ms : it corresponds to a 200Hz refresh rate.
It means we can group updates of deadlines that are within a 5ms timespan.
To do that we also need to decouple GameState update from rendering.

- when terminal is resized, re-center world accordingly.
  - if terminal size is too small, pause the game and display a message

- chose target and numbers such that there is a single possibility to reach that sum,
or such that only one of the number should be ruled out?

- use hspec for testing

- 3..2..1..GO! countdown at the beginning of a level

- animate lose/win messages

- make inter-level animation time independent of compute / render time
-> keep "the last time world evolution was rendered" in UIEvolutions to not make deadlines based on current time

- when expanding, invert animation order between frame and infos

- use a dedicated animation for world swap, and then one for game start

- for world with rectangular shape, the walls become visible at the other side of the fronteer
on animations. Maybe we should animate the world too, or use a scissor of the shape of the morphed
frame when drawing it.

- a mode with limited visibility (a circle around the ship)

- animate colors of "You win / You lost"
using colors of numbers for win / colors of explosions for lost

- for terminal, allow the points to go a little further up so that gravity makes them come back
when shooting upwards

## Configurability

- let users chose:
  - blocksize

## World

- make bigger worlds (whole terminal) with more numbers (maybe duplicate some)
  - precompute some one connected components worlds if it is slow.
  - or change the air/wall ratio

### Random world constraints

- allow multiple connected components, but use the biggest connected component
(in terms of area? number of cells? both?) and put numbers and ship in it.
The other air connected components could be converted to walls.
This relaxes the constraint on having a single connected component.

- prevent "reduced rectangle" effect (i.e. make sure on each side there is Air touching the border)

## Game Notions
- some numbers are hidden in the walls
- 3 lives at the beginning, gain one life if you complete a level in less than 5 seconds.
- walls are destroyable and contain ammo
- frame walls can be destroyed in 2 laser shots
- when 2 rooms are separated by a wall, the wall can be destroyed by using the laser.
hitting a key (the key should be present also in the other room)
- or each room has an objective number, once the objective is reached, a door opens to the next room

## ParticleSystem Design

## Playability
- do not count duplicate laser shots in same motion step, it's "unfair".
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
