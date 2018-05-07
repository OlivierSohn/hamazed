# What is it?

A multi-player game engine and games made with it. [![Build Status](https://travis-ci.org/OlivierSohn/hamazed.svg?branch=master)](https://travis-ci.org/OlivierSohn/hamazed)

The engine allows real-time polyphonic music generation and playback, using a midi-like formalism.
The game server can send note on / note off events to game clients, allowing
to perfectly synchronize the music with game events. For an example, see `imj-game-hamazed`.

It is an experimental project, so the APIs will likely change a lot over time.

# Setup

## Submodules

`imj-bindings-audio` provides bindings to a C++ audio engine,
compiled from source, in submodules.

Hence, be sure to `git submodule init && git submodule update`
after checking out the repo, to avoid errors like:

```shell
/.../imj-bindings-audio/c/library.cpp:7:10: error:
         fatal error: 'cpp.os.logs/source/unity.build.cpp' file not found
      |
    7 | #include "cpp.os.logs/source/unity.build.cpp"
      |          ^
    #include "cpp.os.logs/source/unity.build.cpp"
             ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    1 error generated.
    `gcc' failed in phase `C Compiler'. (Exit code: 1)
```

## Dependencies

For the `FTGL` package to build, you may need to install `ftgl` on your system,
unless it is already there.

- On OSX:

```shell
brew install ftgl
```

- On Linux:

```shell
sudo apt-get update
sudo apt-get install ftgl-dev
```

# Build

You can build using [stack](https://docs.haskellstack.org):

`stack build --pedantic`

# Packages list

Topologically sorted:

- imj-prelude
- imj-bindings-audio
  - Bindings to a C++ audio engine.
- imj-music
  - Polyphonic music scores creation and playback.
- imj-base
  - geometry, text animations
  - rendering:
     - In a GLFW-driven OpenGL window
     - In the terminal, using a "delta renderer" to render incrementally
     without screen tearing.
- imj-measure-stdout
  - A test executable to measure the maximum capacity of stdout, and observe
  the effect of different buffering modes.
- imj-game
  - Multi-player game engine.
- imj-particlesystem (was named imj-animation)
  - Animated particle systems.
- imj-space
  - Creates random 2D game levels, given some topological constraints.
- imj-profile
  - An executable producing a file that is to be embedded in the game executable,
  containing optimal strategies to use for random level generation.
  - Also other profiling tests
- imj-game-hamazed
  - The Hamazed game.
- imj-game-tutorial-increment
  - A tutorial on how to use `imj-engine` to build a multi-player game.

# Demo

A demo of (an older version of) imj-game-hamazed:

[![asciicast](https://asciinema.org/a/156059.png)](https://asciinema.org/a/156059)
