# What is it?

A multiplayer game where each player controls a spaceship flying through numbers.

The goal is to shoot the numbers whose sum will be equal to the level's target.

Higher levels have more and more numbers, and less and less space to navigate through them.

Good Luck!

## Features

- 8-bit colors
- Level musics
- Physics-based animations
- Inter-level animations
- OpenGL and terminal rendering.

# Demos

## No walls, square world

[![asciicast](https://asciinema.org/a/156061.png)](https://asciinema.org/a/156061)

## Random walls, rectangular world

[![asciicast](https://asciinema.org/a/156059.png)](https://asciinema.org/a/156059)

# Configurability

The world is configurable (square or rectangle, with or without random walls)

![Configuration snapshot](images/config.png?raw=true "Configuration")

You can define your own keyboard mapping by modifying the 'translatePlatformEvent' function
defined
[here](https://github.com/OlivierSohn/hamazed/blob/3821705b036f1c5234a913c7675da1484739f1ed/imj-game-hamazed/src/Imj/Game/Hamazed/KeysMaps.hs)
, the default mapping being:
- ship acceleration : [s e d f]
- laser shots       : [j i k l]

# Supported Platforms

Linux, OS X are supported.

Windows is not supported due to
[this](https://ghc.haskell.org/trac/ghc/ticket/7353))

# Build

You can build and run using [stack](https://docs.haskellstack.org):

`stack build --pedantic && stack exec imj-game-hamazed-exe`
