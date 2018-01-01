# What is it?

It's a terminal ascii game.

## The pitch

You fly a ship through numbers. The goal is to shoot
the numbers whose sum will be equal to the level's target.

Higher levels have more and more numbers, and less and less space to navigate through them.

Good Luck!

## What's inside

Contains 8-bit color, physics animations, inter-level animations, and an optimized renderer to avoid screen tearing.

# Demos

## No walls, square world

[![asciicast](https://asciinema.org/a/151434.png)](https://asciinema.org/a/151434)

## Random walls, rectangular world

[![asciicast](https://asciinema.org/a/151404.png)](https://asciinema.org/a/151404)

# Configurability

The world is configurable (square or rectangle, with or without random walls)

![Configuration snapshot](images/config.png?raw=true "Configuration")

You can define your own keyboard mapping by modifying the 'eventFromKey' function
defined
[here](https://github.com/OlivierSohn/hamazed/blob/3821705b036f1c5234a913c7675da1484739f1ed/imj-game-hamazed/src/Imj/Game/Hamazed/KeysMaps.hs)
, the default mapping being:
- ship acceleration : [s e d f]
- laser shots       : [j i k l]

# Supported Platforms / Terminals:

Linux, OS X are supported.

Windows is not supported due to
[this](https://ghc.haskell.org/trac/ghc/ticket/7353))
.

Your terminal window should have a dimension of at least {height = 42, width = 146}.
If it is too small, the program will fail with the following error message:

```
From game thread:

Minimum terminal size : Window {height = 42, width = 146}.
Current terminal size : Window {height = 22, width = 165}.
The current terminal size doesn't match the minimum size,
please adjust your terminal size and restart the executable.
```

# Build

You can build and run using [stack](https://docs.haskellstack.org):

`stack build --pedantic && stack exec hamazed-exe`

# Credits

## Delta rendering

The initial idea for delta rendering is based on [code written by Rafael Ibraim](https://gist.github.com/ibraimgm/40e307d70feeb4f117cd)
