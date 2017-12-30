# What is it?

The monorepo for the game "Hamazed" and its modules.

# Build

You can build using [stack](https://docs.haskellstack.org):

`stack build --pedantic`

You can test using

`stack test`

Note that tests should all pass!

# Libraries : an overview

- imj-base
  - The "engine" : Base classes and types, geometry, delta-rendering, etc...
- imj-animation
  - An animation framework and concrete animations. Uses imj-base.
- imj-game-hamazed
  - The game itself, using the two libraries above.

There are two ways to animate things graphically : Animations and morphings.

Animations and morphings share the fact that they "animate items" across time,
yet they are deeply different in nature w.r.t:
- The specification of their "boundary conditions"
  - Interpolations require "start" and "end" (+ optional waypoints) states to be specified.
  - Animations require only a "start" state to be specified.
- Rules
  - Interpolations must satisfy some rules described in the doc of class DiscreteDistance.
  - Animations don't have to satisfy any rule.
- Number of elements
  - Interpolations are always applied to a single element, and cannot create "new elements".
  - Animations create multiple animated points, each of them can in turn "give birth" to
  other animated points, depending on interactions with the environment.
- Current usage in Hamazed game:
  - Animations typically represent a physical phenomenon happening in the game
  (an explosion, a laser shot, a free fall, an object breaking apart). There are exceptions
  like the animation of geometric figures.
  - Morphings are used to smoothly transform and move UI elements from one level to the next.
