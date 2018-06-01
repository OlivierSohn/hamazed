<!-- TOC depthFrom:1 depthTo:6 withLinks:1 updateOnSave:1 orderedList:0 -->

- [What is it?](#what-is-it)
- [Packages](#packages)
- [Development setup](#development-setup)
	- [Install dependencies (C libraries)](#install-dependencies-c-libraries)
- [Build](#build)
- [Run the games in single-player mode](#run-the-games-in-single-player-mode)
- [Run the games in Multi-player mode](#run-the-games-in-multi-player-mode)
	- [Connect to a running game server](#connect-to-a-running-game-server)
		- [Connect to a Heroku-hosted server](#connect-to-a-heroku-hosted-server)
- [CI](#ci)
- [Game engine highlights](#game-engine-highlights)
	- [Supported platforms](#supported-platforms)
	- [Music](#music)
		- [Notation](#notation)
		- [Playback](#playback)
	- [Rendering](#rendering)

<!-- /TOC -->

# What is it?

Monorepo for a Haskell multi-player game engine and games made with it.
[![Build Status](https://travis-ci.org/OlivierSohn/hamazed.svg?branch=master)](https://travis-ci.org/OlivierSohn/hamazed)

The games can be played locally, in single-player mode, or in multi-player mode, after having run the [deployment script]
to host the game server on [Heroku].

You can even create your own multi-player game, starting from the [tutorial-game](/imj-game-tutorial-increment).

# Packages

List of packages, inverse-topologically sorted wrt dependencies, with keywords / short description for each of them:

- [imj-bindings-audio]
  - Bindings to a C++14 audio engine. The C++ sources are located in [submodules](/imj-bindings-audio/c).
- [imj-music](/imj-music)
  - Polyphonic music scores creation and playback.
- [imj-prelude](/imj-prelude)
  - Prelude library used by packages hereunder, mainly to factorize imports.
- [imj-time]
  - Timing library based on the system monotonic clock.
- [imj-base]
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
  - Using [websockets] to communicate between server and clients.
  - Broadcast messages to all clients
  - Handle connection failures generically (so that a client handler, when broadcasting a message,
    won't have to handle exceptions due to another client's connection being down.)
  - Detect client reconnection (keep track of clients identities using their MAC address)
  - Logging
- [imj-serve-highscores]
  - [Servant]-based web service to persist and access highscores.
- [imj-game]
  - Multi-player game engine
  - Listens to server events, and player events
  - [Handles generic events](/imj-game/src/Imj/Game/Update.hs), so that the game implementation
  contains only game-specific code.
    - Real-time music playback.
  - Debugging features : record and display events graphically, event logging.
- [imj-game-tutorial-increment](/imj-game-tutorial-increment)
  - A tutorial on how to use [imj-game] to build a multi-player game.
- [imj-game-synths]
  - "A jam session, in the cloud, with loops and synthesizers."
  - Players are playing music together, in real-time, using their computer keyboards as synthesizers.
- [imj-game-hamazed]
  - You're a ship pilot, shooting at flying numbers.
  - Levels generation is randomized, so that you never play the same game twice.
	- With 4 different level musics.
  - Demo (when the game was mono-player and had no music):
      [![asciicast](https://asciinema.org/a/156059.png)](https://asciinema.org/a/156059)
- [imj-profile](/imj-profile)
  - An executable precomputing optimal strategies used for random level generation.
  - And other profiling tests.

# Development setup

After checking out the repo, run `git submodule init && git submodule update` to download the submodules.

## Install dependencies (C libraries)

- [ftgl] is needed by [imj-base] to render fonts with OpenGL.
- [portaudio 19] is needed by [imj-bindings-audio] for audio I/O.
- [libpq] and [postgresql] are needed by a dependency of [imj-serve-highscores].

To install them:

- On OSX:

```shell
brew install ftgl
brew install portaudio
brew install postgresql
```

- On Linux:

```shell
sudo apt-get update
sudo apt-get install ftgl-dev
sudo apt-get install portaudio19-dev
sudo apt-get install postgresql
sudo apt-get install libpq-dev
```

# Build

[stack] is the preferred tool to build the project:

`stack build --pedantic`

A recent enough C compiler should be used by GHC, so as to be able to build C++14.

# Run the games in single-player mode

Passing no command line argument will run the games in single player mode:

`stack exec <game-executable>`

# Run the games in Multi-player mode

Use the [deployment script] to host the games on a [Heroku] server.

## Connect to a running game server

```shell
stack exec -- <game-executable> -n <serverName> -p<serverPort>
```

### Connect to a Heroku-hosted server

When the game server is hosted on [Heroku], the port to connect to is `80`:

```shell
stack exec -- <game-executable> -n <herokuAppDomain> -p80
```

# CI

The [CI script](/.travis.yml) verifies that compilation and tests succeed with
GHC versions 8.2.2 and 8.4.2.


# Game engine highlights

## Supported platforms

Supported client platforms are OSX and Linux (but see the rendering limitation).

## Music

### Notation

[Melodies](/imj-game-hamazed/src/Imj/Game/Hamazed/Music.hs)
are written using the `notes` quasiquoter, where:

- notes names follow [the solfege notation]
- a note can be shifted by octaves using `v` and `^`
- `-` extends the preceding note
- `.` indicates a pause

### Playback

Every game made with [imj-game] can have the server send midi-like
note on / note off events to game clients, allowing to perfectly synchronize the music with the game.

The music won't pause during garbage collection because we use
[a custom-made audio engine](/imj-bindings-audio) whose audio thread is
not managed by the GHC runtime.

## Rendering

The screen is conceptually divided in small blocks of equal size,
where each block can contain a character with 8-bit background and foreground colors.

The [fonts](/imj-base/fonts) and font size for rendering can be modified at run time.

[deployment script]: ./deploy-heroku.sh
[ftgl]: http://ftgl.sourceforge.net/docs/html/
[Heroku]: https://www.heroku.com/
[imj-base]: /imj-base
[imj-bindings-audio]: /imj-bindings-audio
[imj-game]: /imj-game
[imj-game-hamazed]: /imj-game-hamazed
[imj-game-synths]: /imj-game-synths
[imj-serve-highscores]: /imj-serve-highscores
[imj-time]: /imj-time
[libpq]: https://www.postgresql.org/docs/9.5/static/libpq.html
[postgresql]: https://www.postgresql.org/
[portaudio 19]: http://www.portaudio.com/
[Servant]: http://haskell-servant.readthedocs.io/en/stable/
[stack]: https://docs.haskellstack.org
[the solfege notation]: https://en.wikipedia.org/wiki/Solf%C3%A8ge#Fixed_do_solf%C3%A8ge
[websockets]: http://hackage.haskell.org/package/websockets
