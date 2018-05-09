# What is it?

Monorepo containing a Haskell multi-player game engine with real-time polyphonic music,
and some games made with it. [![Build Status](https://travis-ci.org/OlivierSohn/hamazed.svg?branch=master)](https://travis-ci.org/OlivierSohn/hamazed)

Note that it is an experimental project, so the APIs will likely change a lot over time.

# Packages

Packages inverse-topologically sorted wrt dependencies, with some keywords for each of them:

- [imj-bindings-audio](/imj-bindings-audio)
  - Bindings to a C++ audio engine (submodules contain the C++ sources).
- [imj-music](/imj-music)
  - Polyphonic music scores creation and playback.
- [imj-prelude](/imj-prelude)
- [imj-base](/imj-base)
  - Containers (Graph, Matrix, Cyclic matrix, Dynamic vector, etc...)
  - Geometry, text animations
  - 8-bit color manipulation in different color spaces
  - Interpolations / Morphings
  - Physics
  - Rendering backends:
     - In a GLFW-driven OpenGL window
     - Delta-rendering in the terminal, to avoid screen tearing
- [imj-space](/imj-space)
  - Creates random 2D game levels, given some topological constraints.
- [imj-particlesystem](/imj-particlesystem) (formerly `imj-animation`)
  - Physics-based and geometric particle systems.
- [imj-measure-stdout](/imj-measure-stdout)
  - An executable to measure the maximum capacity of stdout, and observe
  the effect of different buffering modes.
- [imj-server](/imj-server)
  - Using [websockets](http://hackage.haskell.org/package/websockets).
  - Broadcast messages to all clients
  - Detect client reconnection
  - Logging
- [imj-game](/imj-game)
  - Multi-player game engine.
- [imj-game-hamazed](/imj-game-hamazed)
  - The 'Hamazed' game (see the demo below).
- [imj-game-tutorial-increment](/imj-game-tutorial-increment)
  - A tutorial on how to use [imj-base](/imj-base) to build a multi-player game.
- [imj-profile](/imj-profile)
  - An executable precomputing optimal strategies used for random level generation.
  - + other profiling tests.

# Music

Every game using [imj-game](/imj-game) will be able to play music! The game server can send
midi-like note on / note off events to game clients, allowing
to perfectly synchronize the music with game events.

Writing melodies can be done using the dedicated `notes` quasiquoter:

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

where :

- notes names follow [this](https://en.wikipedia.org/wiki/Solf%C3%A8ge#Fixed_do_solf%C3%A8ge) notation
- `v` and `^` represent down / up one octave for the value that follows
- `.` indicates a pause
- `-` extends the preceding value.

More score examples are available [here](/imj-game-hamazed/src/Imj/Game/Hamazed/Music.hs)

# Rendering

The screen is conceptually divided in small blocks of equal size,
where each block can contain a character with 8-bit background and foreground colors.

The font and font size used to render can be modified at runtime.

# Demo

An demo of imj-game-hamazed (at a time when the game was mono-player and had no music yet):

[![asciicast](https://asciinema.org/a/156059.png)](https://asciinema.org/a/156059)

# Command line options

Every game made with [imj-game](/imj-game) inherits from these command line options:

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

# Build

After checking out the repo, run `git submodule init && git submodule update` to download the submodules.

Unless it is already there, install the `ftgl` library on your system
(it is needed for Font handling) :

- On OSX:

```shell
brew install ftgl
```

- On Linux:

```shell
sudo apt-get update
sudo apt-get install ftgl-dev
```

Build the project using [stack](https://docs.haskellstack.org):

`stack build --pedantic`

# Supported GHC versions

GHC versions 8.2.2 and 8.4.1 are supported.

# Travis CI configuration

To build [the audio engine package](/imj-bindings-audio/imj-bindings-audio.cabal),
GHC needs to use a C++14 compiler.

The Travis image for linux is Ubuntu lts-14 whose `gcc` doesn't have c++14, so
the [travis script](/.travis.yml) takes care of installing a newer `gcc`
(`g++-7` and `libstdc++-7-dev`), setting environment variables for compilers
and changing the `gcc` symbolink link to point to the new one.

Note that on OSX, the travis image contains a `clang` version recent enough that
it supports c++14 so nothing special had to be done.

## Allowed failures

On Linux with GHC versions < 8.4.1, the build fails for
[this](https://github.com/commercialhaskell/stack/issues/3472) reason,
hence the [travis script](/.travis.yml) allows Linux to fail for ghc versions < 8.4.1.

# Run the games

Run the tutorial game:

`stack exec imj-game-tutorial-increment-exe`

Run Hamazed game:

`stack exec imj-game-hamazed-exe `

To pass some command line arguments, `--` needs to be written after `exec`:

`stack exec -- imj-game-hamazed-exe -l console`
