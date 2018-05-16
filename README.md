# What is it?

Monorepo containing a Haskell multi-player game engine with real-time polyphonic music,
and some games made with it. [![Build Status](https://travis-ci.org/OlivierSohn/hamazed.svg?branch=master)](https://travis-ci.org/OlivierSohn/hamazed)

Note that it is an experimental project, so the APIs will likely change a lot over time.

# Packages

Packages inverse-topologically sorted wrt dependencies, with some keywords for each of them:

- [imj-bindings-audio](/imj-bindings-audio)
  - Bindings to a C++14 audio engine. The C++ sources are located in [submodules](/imj-bindings-audio/c).
- [imj-music](/imj-music)
  - Polyphonic music scores creation and playback.
- [imj-prelude](/imj-prelude)
  - Prelude library used by packages hereunder, mainly to factorize imports.
- [imj-base](/imj-base)
  - Containers (Graph, Matrix, Cyclic matrix, Dynamic vector, etc...)
  - Geometry, text animations
  - 8-bit color manipulation in different color spaces
  - Easing, inverse-easing, interpolation, rectangular frame morphing.
  - Physics
  - UI components building blocks
    - Chat
  - Rendering backends, using [delta-rendering](/imj-base/src/Imj/Graphics/Render/Delta.hs):
     - In a GLFW-driven OpenGL window
     - In the terminal
  - Player input, window management.
- [imj-space](/imj-space)
  - Creates random 2D game levels, given some topological constraints.
- [imj-particlesystem](/imj-particlesystem) (formerly `imj-animation`)
  - Physics-based and geometric particle systems.
- [imj-measure-stdout](/imj-measure-stdout)
  - An executable to measure the maximum capacity of stdout, and observe
  the effect of different buffering modes.
- [imj-server](/imj-server)
  - Using [websockets](http://hackage.haskell.org/package/websockets) to communicate between server and clients.
  - Broadcast messages to all clients
  - Handle connection failures generically (so that a client handler, when broadcasting a message,
    won't have to handle exceptions due to another client's connection being down.)
  - Detect client reconnection (keep track of clients identities using their MAC address)
  - Logging
- [imj-game](/imj-game)
  - Multi-player game engine
  - Listens to server events, and player events
  - [Handles generic events](/imj-game/src/Imj/Game/Update.hs), so that the game implementation
  contains only game-specific code.
  - Debugging features : record and display events graphically, event logging.
- [imj-game-hamazed](/imj-game-hamazed)
  - The 'Hamazed' game (see the demo below).
- [imj-game-tutorial-increment](/imj-game-tutorial-increment)
  - A tutorial on how to use [imj-game](/imj-game) to build a multi-player game.
- [imj-profile](/imj-profile)
  - An executable precomputing optimal strategies used for random level generation.
  - And other profiling tests.

# Music

## Playback

Every game made using [imj-game](/imj-game) is able to play music: the game server
sends midi-like note on / note off events to game clients, allowing
to perfectly synchronize the music with game events.

### Synthesizers continue playing during garbage collection pauses

We use [a custom-made audio engine](/imj-bindings-audio) whose audio thread is
/not/ managed by the GHC runtime, and "synthesizers" are creating the sound in the
audio thread directly.

Hence, the sound is guaranteed /not/ to pause during garbage collection pauses.

## Notation

[Melodies](/imj-game-hamazed/src/Imj/Game/Hamazed/Music.hs)
are written using the `notes` quasiquoter, where:

- notes names follow [the solfege notation](https://en.wikipedia.org/wiki/Solf%C3%A8ge#Fixed_do_solf%C3%A8ge)
- a note can be shifted by octaves using `v` and `^`
- `-` extends the preceding note to the next step
- `.` indicates a pause

# Rendering

The screen is conceptually divided in small blocks of equal size,
where each block can contain a character with 8-bit background and foreground colors.

The [fonts](/imj-base/fonts) and font size for rendering can be modified at runtime.

# Demo

This is imj-game-hamazed, at a time when the game was mono-player and had no music yet:

[![asciicast](https://asciinema.org/a/156059.png)](https://asciinema.org/a/156059)

# Development Setup

After checking out the repo, run `git submodule init && git submodule update` to download the submodules.

## Dependencies

As you can see in the CI scripts ([here](/.travis.yml) and [there](/Brewfile)),
some c libraries are expected to be installed on your system
in order to build the game engine:

- [ftgl](http://ftgl.sourceforge.net/docs/html/) is needed by [imj-base](/imj-base)
to render fonts with OpenGL.
- [portaudio 19](http://www.portaudio.com/) is needed by [imj-bindings-audio](/imj-bindings-audio)
for audio I/O.

To install them:

- On OSX:

```shell
brew install ftgl
brew install portaudio
```

- On Linux:

```shell
sudo apt-get update
sudo apt-get install ftgl-dev
sudo apt-get install portaudio19-dev
```

# Build

## With stack

[stack](https://docs.haskellstack.org) is the preferred tool to build the project:

`stack build --pedantic`

## With other tools

You may want to build with another tool, in that case be sure to read the
[stack.yaml](/stack.yaml) and manually download the right versions of packages
listed in it:

- hackage packages listed in the `extra-deps` field
- non-hackage packages listed in the `packages` field with the following syntax:

```yaml
- location:
    git: https://github.com/OlivierSohn/clay.git
    commit: 954476940190873094847939c5f0e6242e324000
  extra-dep: true
```

Meaning that the `clay` package will be downloaded from `https://github.com/OlivierSohn/clay.git`
at commit `954476940190873094847939c5f0e6242e324000`.

# Command line options

Every game made with [imj-game](/imj-game) inherits from the same command line options.

The description of client-specific options are prefixed by [Client]:

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
  -n,--serverName ARG      Connect to a server (use "0.0.0.0" to target your
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

# Run the games

## Single-player, local-server

Run the tutorial game:

`stack exec imj-game-tutorial-increment-exe`

Run Hamazed game:

`stack exec imj-game-hamazed-exe `

To pass some command line arguments, `--` needs to be written after `exec`:

`stack exec -- imj-game-hamazed-exe -l console`

## Multi-player, deployed-server

To allow several players to play the same game instance, the game server should be hosted in the cloud.

At the time of this writing, [Heroku](https://www.heroku.com/) has a free plan to host a web application
with no location restriction (it is ok for users located in Europe), and accepting several deployment
methods, including docker containers, which is the approach described here:

### Create the docker container for the game server

First build the base image:

```shell
cd ./docker-rt
docker build -t imajuscule/imj-game-rt .
```

(Note that the name of the image must match the one used in the .yaml file)


Then build the imajuscule/hamazed image:

```shell
cd ..
stack image container
```

Optionally, you can build the image locally:

```shell
docker build -t imajuscule/game-synths-tests .
```

Optionally, you can test the image locally, overriding the -pPORT in the Dockerfile (see -p10052)
because Heroku uses the PORT env variable to convey the information on port number.

```shell
docker run imajuscule/imj-game-synths-test -p10052
```

Create the container for heroku and push it to heroku
(assuming you already have an heroku account, have created an app <herokuAppName>, and have installed
heroku command line tools):

```shell
heroku container:push web -a <herokuAppName>
```

You can verify the deployment status and game-server state using

```shell
heroku logs --tail -a <herokuAppName>
```

Note that in the Dockerfile, we pass `-l console` so that the server logs in the console, it allows
to command above to provide relevant applicative logs.

Finally, players can connect to the game server:

```shell
stack exec -- imj-game-synths-exe -n <herokuAppDomain> -p80
```

Note that the http port is used (this is the only available port on Heroku)

If you access <herokuAppDomain> from a browser, you will get an error because the game server sockets
are using the websocket protocol. Hence, you should see in the logs an error containing "Header missing: Sec-WebSocket-Key".

# CI

GHC versions 8.2.2 and 8.4.1 are supported.

## Travis script

### Building C++14 with GHC on Ubuntu 14.04 LTS (Trusty)

To build [the audio engine package](/imj-bindings-audio/imj-bindings-audio.cabal),
GHC needs to use a C++14 compiler.

The Travis image for linux is Ubuntu 14.04 LTS (Trusty) whose `gcc` doesn't have c++14, so
the [travis script](/.travis.yml) takes care of installing a newer `gcc`
(`g++-7` and `libstdc++-7-dev`), setting environment variables for compilers
and changing the `gcc` symbolink link to point to the new one.

Note that on OSX, the travis image contains a `clang` version recent enough that
it supports c++14 so nothing special had to be done.

### Allowed failures

On Linux with GHC versions < 8.4.1, the build fails for
[this](https://github.com/commercialhaskell/stack/issues/3472) reason,
hence the [travis script](/.travis.yml) allows Linux to fail for ghc versions < 8.4.1.
