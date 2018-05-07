# What is it?

A multi-player game engine, with real-time polyphonic music, and some games made with it. [![Build Status](https://travis-ci.org/OlivierSohn/hamazed.svg?branch=master)](https://travis-ci.org/OlivierSohn/hamazed)

A tutorial `imj-game-tutorial-increment` shows how to use `imj-engine` to build
a minimalistic multi-player game. You can start from there to build your own!

If you need a particular feature for your game, and it's not yet in the engine,
we can discuss it on a gitlab issue. I can also provide some support if you have questions.

Note that it is an experimental project, so the APIs will likely change a lot over time.

# Music

The game server can send midi-like note on / note off events to game clients, allowing
to perfectly synchronize the music with game events. For an example, see `imj-game-hamazed`.

Writing simple melodies can be done easily using the dedicated `notes` quasiquoter:

```haskell
[notes|
  do . .
  . . sol
  ré - -
  - mib fa
  sol mib do
  ré . v sol
  do . .
  |]
```

where notes names follow [this](https://en.wikipedia.org/wiki/Solf%C3%A8ge#Fixed_do_solf%C3%A8ge) notation,
`v` and `^` represent down / up one octave for the value that follows,
`.` indicates a pause,
and `-` extends the preceding value.

# Submodules

After checking out the repo, be sure to `git submodule init && git submodule update`,
to avoid build errors like:

```shell
/.../imj-bindings-audio/c/library.cpp:7:10: error:
         fatal error: 'cpp.os.logs/source/unity.build.cpp' file not found
      |
    7 | #include "cpp.os.logs/source/unity.build.cpp"
      |          ^
    #include "cpp.os.logs/source/unity.build.cpp"
             ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    1 error generated.
    'gcc' failed in phase 'C Compiler'. (Exit code: 1)
```

# Dependencies

To build a dependency (the `FTGL` package), `ftgl` must be installed on your system.

You can install it (unless it is already there):

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

Using an inverse topological sort on the dependency graph:

- `imj-prelude`
- `imj-bindings-audio`
  - Bindings to a C++ audio engine (submodules contain the C++ sources).
- `imj-music`
  - Polyphonic music scores creation and playback.
- `imj-base`
  - geometry, text animations
  - rendering:
     - In a GLFW-driven OpenGL window
     - In the terminal, using a "delta renderer" to render incrementally
     without screen tearing.
- `imj-measure-stdout`
  - A test executable to measure the maximum capacity of stdout, and observe
  the effect of different buffering modes.
- `imj-game`
  - Multi-player game engine.
- `imj-particlesystem` (was named `imj-animation`)
  - Animated particle systems.
- `imj-space`
  - Creates random 2D game levels, given some topological constraints.
- `imj-profile`
  - An executable producing a file that is to be embedded in the game executable,
  containing optimal strategies to use for random level generation.
  - Also other profiling tests
- `imj-game-hamazed`
  - The Hamazed game.
- `imj-game-tutorial-increment`
  - A tutorial on how to use `imj-engine` to build a multi-player game.

# Demo

A demo of (an older version of) imj-game-hamazed:

[![asciicast](https://asciinema.org/a/156059.png)](https://asciinema.org/a/156059)
