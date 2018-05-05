# What is it?

A multi-player game engine featuring:

- An architecture where you just need to concentrate on the core logic of your game,
the rest being handled by the engine.
- A chat window for inter-player communications, and commands launching (you can
  define custom commands for your game).
- The possibility to play midi-like music during the game, with very low overhead,
using a C++ sound engine with simple synthesizers. The sound continues during GC pauses,
because the audio real-time thread is not handled by the GHC runtime.
- Prioritized deadlines, so that the game feels responsive at all times.
- Character-based rendering with different fonts the user can chose from.
- Flexible configuration, through command line parameters:
  - Chose to run just the server, just the client, or both in the same process.
  - Colored server-side logging
  - Client-side logging + graphical representation of handled events for debugging purposes
  - Chose render target (render in the terminal or in a separate, glfw-driven window)
  - Audio silencing

To get a feel of how to use the engine, take a look at `imj-game-tutorial-increment`
which features a very simple multi-player "game" where every player can increment
a global, shared counter, by pressing the space bar.

For a more real-life example, look at `imj-game-hamazed` which is the game that gave
birth to this game-engine.
