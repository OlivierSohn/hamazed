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

[stack](https://docs.haskellstack.org) is the preferred tool to build the project:

`stack build --pedantic`

# Single-player mode

Passing no command line argument will run the game in single player mode:

- The tutorial game:

`stack exec imj-game-tutorial-increment-exe`

- Hamazed game:

`stack exec imj-game-hamazed-exe `

# Multi-player mode

In multi-player mode, a game server synchronizes the game, and clients are connected to it.

## Deploying a game server

### Dockerize the game server

Build the base image:

```shell
cd ./docker-rt
docker build -t imajuscule/imj-game-rt .
```

Insert the executables in the image:

```shell
cd ..
stack image container
```

For dependencies to be consistent, the system on which the command above is executed
should match the characteristics of [the base image](./docker-rt/Dockerfile).

As a result, the generated image `imajuscule/hamazed` now contains the game executables.

In the following section, `imajuscule/hamazed` is used as a base to generate another image
that will be deployed on Heroku.

### Deploying a game server to Heroku

At the time of this writing, [Heroku](https://www.heroku.com/) has a free plan to host web applications
deployed using docker containers.

We are assuming that you already:

- have an heroku account
- have created an app <herokuAppName>
- and have installed heroku command line tools

(Optional) Build and test the heroku container locally:

```shell
docker build -t imajuscule/game-synths-tests .

# When testing locally, we need overriding the -p parameter because
# when running on Heroku, the port number is deduced from the `PORT` env variable.
docker run imajuscule/imj-game-synths-test -p10052
```

Create the heroku container and push it on heroku. This can take a while the first time, but
once the initial upload is done, the following uploads will be fast because the parts (layers)
of the docker image that didn't change won't be uploaded.

```shell
heroku container:push web -a <herokuAppName>
```

Monitor / verify the deployment status:

```shell
heroku logs --tail -a <herokuAppName>
```

## Connecting a client to a running game server

Once the server is deployed, clients may connect to it by IP/name and port:

```shell
stack exec -- imj-game-synths-exe -n <serverName> -p<serverPort>
```

### on Heroku

When the game server is hosted on Heroku the port to connect to is `80`:

```shell
stack exec -- imj-game-synths-exe -n <herokuAppDomain> -p80
```

Note that if you try accessing `<herokuAppDomain>` from a browser, you will get an error,
and looking at the logs you'll see "Header missing: Sec-WebSocket-Key".
This is because the game server sockets are using the websocket protocol, not the HTTP protocol.
