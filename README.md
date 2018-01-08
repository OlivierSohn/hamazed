# What is it?

The monorepo for the game "Hamazed" and its packages. [![Build Status](https://travis-ci.org/OlivierSohn/hamazed.svg?branch=master)](https://travis-ci.org/OlivierSohn/hamazed)

The haddock documentation explains the concepts used, both on the game side and
on the game engine side.

## Demo

  [![asciicast](https://asciinema.org/a/151404.png)](https://asciinema.org/a/151404)

# Packages list

- imj-prelude
  - The prelude I use in other packages.
- imj-base

It is an "engine" library: it contains

  - base classes and types, geometry, text animations,
  - a "delta renderer" rendering in the terminal without screen tearing,
  - `imj-base-examples-exe`, a text animation demo:

  [![asciicast](https://asciinema.org/a/156054.png)](https://asciinema.org/a/156054)

- imj-particlesystem
  - A library to create animations.
- imj-game-hamazed
  - The game, using the two libraries above.
- imj-measure-stdout
  - A test application to measure the maximum capacity of stdout, and observe the effect
  of different buffering modes.

# Contributions

Contributions are welcome. You can design new animations, new games, enhance the
current features, etc...

# Build

You can build using [stack](https://docs.haskellstack.org):

`stack build --pedantic`

And test using

`stack test --pedantic`

# Concepts comparisons

Here I underline some similarities and differences between different concepts
I introduced in the libraries : interpolation, animation and morphing.

## A comparison between DiscreteInterpolation and DiscreteMorphing / DiscreteColorableMorphing

DiscreteInterpolation interpolates between values :
the result of the interpolation between 2 'v' is a 'v'.

DiscreteMorphing, DiscreteColorableMorphing interpolates between graphical representations of a value:
the result of a morphing between 2 'v' is a graphical representation
"somewhat in-between" the 2 graphical representations of the 'v'.

## A comparison between Animations and Morphings

There are two ways to "animate things graphically" : Animations (ParticleSystem in imj-particlesystem)
and morphings (DiscreteMorphing, DiscreteColorableMorphing in imj-base).

Both share the fact that they "animate items" across time, yet they are deeply different in
nature w.r.t:
- The specification of their "boundary conditions"
  - Morphings require "start" and "end" (+ optional waypoints) states to be specified.
  - Animations require only a "start" state to be specified.
- Number of elements
  - Morphings are always applied to a single element, and cannot create "new elements".
  - Animations create multiple particles, each of them can in turn "give birth" to
  other particles, depending on interactions with the environment.
- Current usage in Hamazed game:
  - Animations typically represent a physical phenomenon happening in the game
  (an explosion, a laser shot, a free fall, an object breaking apart).
  - Morphings are used to smoothly transform and move UI elements from one level to the next.
