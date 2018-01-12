# What is it?

The monorepo for the game "Hamazed" and its packages. [![Build Status](https://travis-ci.org/OlivierSohn/hamazed.svg?branch=master)](https://travis-ci.org/OlivierSohn/hamazed)

The haddock documentation explains the concepts used, both on the game side and
on the game engine side.

## Demo

[![asciicast](https://asciinema.org/a/156059.png)](https://asciinema.org/a/156059)

# Packages list

- imj-prelude
  - The prelude I use in other packages.
- imj-base
The "engine" library containing:
  - base classes and types,
  - geometry, text animations,
  - a "delta renderer" rendering in the terminal without screen tearing,
  - `imj-base-examples-exe`, a text animation demo:

  [![asciicast](https://asciinema.org/a/156054.png)](https://asciinema.org/a/156054)

- imj-particlesystem
  - A library to create animated particle systems.
- imj-game-hamazed
  - The game
- imj-measure-stdout
  - A test application to measure the maximum capacity of stdout, and observe the effect
  of different buffering modes.

# Contributions

Contributions are welcome!

# Build

You can build using [stack](https://docs.haskellstack.org):

`stack build --pedantic`

And test using

`stack test --pedantic`
