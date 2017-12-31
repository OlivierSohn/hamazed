# What is it?

The monorepo for the game "Hamazed" and its packages.

To get familiar with the concepts, I recommend browsing the haddock documentation:
it is up-to-date, structured, and quite exhaustive, both on the libraries side
and on the game side.

# Packages

- imj-prelude
  - The prelude I use in other packages.
- imj-base
  - An "engine" library: it contains base classes and types, geometry, delta-rendering, etc...
  - Also contains an executable that can be run to see examples (at the time of writing,
    examples about text animation).
- imj-animation
  - A library to create animations. Uses imj-base.
- imj-game-hamazed
  - An application (the game), using the two libraries above.

With imj-base and imj-animation you can start a game of your own,
that's the idea behind making this project open-source.

Either you can just use the delta renderer ("Imj.Graphics.Render.Delta") which will
ensure that your games runs wihout screen tearing, or you can also
use the other concepts of imj-base and imj-animations.

# Contributions

Contributions are welcome, if you want you can open an issue to discuss it beforehand,
or directly open a PR.

The code compiles with --pedantic, please make sure it compiles with it before
submitting a PR.

# Build

You can build using [stack](https://docs.haskellstack.org):

`stack build --pedantic`

And test using

`stack test --pedantic`

# Misc.

## A comparison between DiscreteInterpolation and DiscreteMorphing / DiscreteColorableMorphing

DiscreteInterpolation interpolates between values :
the result of the interpolation between 2 'v' is a 'v'.

DiscreteMorphing, DiscreteColorableMorphing interpolates between graphical representations of a value:
the result of a morphing between 2 'v' is a graphical representation
"somewhat in-between" the 2 graphical representations of the 'v'.

## A comparison between Animations and Morphings (notions used in the libraries)

There are two ways to "animate things graphically" : Animations (Animation in imj-animation)
and morphings (DiscreteMorphing, DiscreteColorableMorphing in imj-base).

Both share the fact that they "animate items" across time, yet they are deeply different in
nature w.r.t:
- The specification of their "boundary conditions"
  - Morphings require "start" and "end" (+ optional waypoints) states to be specified.
  - Animations require only a "start" state to be specified.
- Number of elements
  - Morphings are always applied to a single element, and cannot create "new elements".
  - Animations create multiple animated points, each of them can in turn "give birth" to
  other animated points, depending on interactions with the environment.
- Current usage in Hamazed game:
  - Animations typically represent a physical phenomenon happening in the game
  (an explosion, a laser shot, a free fall, an object breaking apart).
  - Morphings are used to smoothly transform and move UI elements from one level to the next.
