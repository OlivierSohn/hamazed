# What is it?

The monorepo for `imj-engine`, a multi-player game engine with real-time polyphonic music,
and some games made with it. [![Build Status](https://travis-ci.org/OlivierSohn/hamazed.svg?branch=master)](https://travis-ci.org/OlivierSohn/hamazed)

A tutorial `imj-game-tutorial-increment` shows how to use `imj-engine` to build
a minimalistic multi-player game. You can start from there to build your own!

If you need a particular feature for your game, and it's not yet in the engine,
we can discuss it on a gitlab issue. I can also provide some support if you have questions.

Note that it is an experimental project, so the APIs will likely change a lot over time.

# Command line options

Every option listed hereunder is available by default for any game made with
`imj-engine`:

```shell
> stack exec -- imj-game-hamazed-exe -h

**** imj-game-hamazed-exe runs a multiplayer client/server game.

Usage: imj-game-hamazed-exe [-s|--serverOnly] [-n|--serverName ARG]
                            [-p|--serverPort ARG] [-l|--serverLogs ARG]
                            [-c|--colorScheme ARG] [--connectId ARG]
                            [-r|--render ARG] [--ppu ARG] [--screenSize ARG]
                            [-d|--debug] [--silent]
  If you want to: (1) Create just a game server, use [--serverOnly] and
  optionally [--serverPort]. (2) Create just a client connected to an existing
  game server, use [--serverName] and optionally [--serverPort]. (3) Create both
  a game server and a client connected to it, use optionally [--serverPort].

Available options:
  -h,--help                Show this help text
  -s,--serverOnly          Create - only - the server (no client). Incompatible
                           with --serverName.
  -n,--serverName ARG      Connect to a server (use "localhost" to target your
                           machine). Incompatible with --serverOnly.
  -p,--serverPort ARG      Listening port number of the server to connect to, or
                           to create. Default is 10052.
  -l,--serverLogs ARG      'none': no server logs. 'console': server logs in the
                           console. Default is 'none'. Incompatible with
                           --serverName.
  -c,--colorScheme ARG     Defines a rgb color from which player colors are
                           deduced, cycling through same intensity colors.
                           Possible values are:
                           {'blue','olive','orange','reddish','violet'}, 'rgb' |
                           '"r g b"' where r,g,b are one of {0,1,2,3,4,5},
                           'time' to chose colors based on server start time.
                           Default is 322 / "3 2 2". Incompatible with
                           --serverName.
  --connectId ARG          [Client] The connection identifier used to connect to
                           the server.
  -r,--render ARG          [Client] 'console': play in the console. 'opengl':
                           play in an opengl window (default value). Accepted
                           synonyms of 'console' are 'ascii', 'term',
                           'terminal'. Accepted synonyms of 'opengl' are 'win',
                           'window'.
  --ppu ARG                [Client OpenGL] The size of a game element, in
                           pixels: '"w h"' where w,h are even and >= 4. Default:
                           "12 8".
  --screenSize ARG         [Client OpenGL] The size of the opengl window.
                           'full': fullscreen. '"width height"' : size in
                           pixels. Default: "600 1400".
  -d,--debug               [Client] print debug infos in the terminal.
  --silent                 [Client] disables music and audio effects.
```

# Music

Every game using `imj-engine` will be able to play music! The game server can send
midi-like note on / note off events to game clients, allowing
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

# Packages list

In inverse-topological sort order (wrt dependencies):

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
  - Multi-player game engine using [websockets](http://hackage.haskell.org/package/websockets)
  for networking.
- `imj-particlesystem` (was named `imj-animation`)
  - Animated particle systems.
- `imj-space`
  - Creates random 2D game levels, given some topological constraints.
- `imj-profile`
  - An executable producing a file that is to be embedded in the game executable,
  containing optimal strategies to use for random level generation.
  - Also other profiling tests
- `imj-game-hamazed`
  - The 'Hamazed' game (see the demo below).
- `imj-game-tutorial-increment`
  - A tutorial on how to use `imj-engine` to build a multi-player game.

# Demo

An older version of imj-game-hamazed (at that time the game was mono-player and had no music):

[![asciicast](https://asciinema.org/a/156059.png)](https://asciinema.org/a/156059)

# How to build

## Submodules

After checking out the repo, be sure to `git submodule init && git submodule update`.

## Dependencies

The rendering depends on the `FTGL` package for font rendering, which is a binding
to `ftgl` library. Hence, `ftgl` must be installed on your system.

Unless it is already there, you can install `ftgl` this way:

- On OSX:

```shell
brew install ftgl
```

- On Linux:

```shell
sudo apt-get update
sudo apt-get install ftgl-dev
```

## Build

Using [stack](https://docs.haskellstack.org):

`stack build --pedantic`

Build errors containing `file not found` error message indicate that submodules are missing.

If you want to build with another tool than stack, please take a look at [stack.yaml](/stack.yaml)
to see which packages will be needed (some forks are not on hackage).

The CI tests that GHC versions >= 8.2.2 are supported : [.travis.yml](/.travis.yml).

# Run games

Run the tutorial game:
`stack exec imj-game-tutorial-increment-exe`

Run Hamazed game:
`stack exec imj-game-hamazed-exe `

Run with some command line args:
`stack exec -- imj-game-hamazed-exe -l console`
(Note the `--` passed after `exec`)
