# What is it?

The monorepo for the game "Hamazed" and its modules.

# Build

You can build using [stack](https://docs.haskellstack.org):

`stack build --pedantic`

You can test using

`stack test`

Note that tests should all pass!

# Libraries : who does what?

TODO add an image of dependency graph (use graphmod).

- imj-animation
  - Animation framework.
- imj-animation
  - Concrete animations.

Animations and interpolations share the fact that they "animate items" across time,
yet they are deeply different in nature w.r.t:
- The specification of their "boundary conditions"
  - Interpolations require "start" and "end" (+ optional waypoints) states to be specified.
  - Animations require only a "start" state to be specified.
- Rules
  - Interpolations must satisfy some rules described in the doc of class DiscretelyInterpolable.
  - Animations don't have to satisfy any rule.
- Number of elements
  - Interpolations are always applied to a single element, and cannot create "new elements".
  - Animations create multiple animated points, each of them can in turn "give birth" to
  other animated points, depending on interactions with the environment.
- Usage (in Hamazed game)
  - Interpolations are used to smoothly transform and move UI elements from one level to the next.
  - Animations are used to represent the destruction of world elements, and to animate laser shots.
