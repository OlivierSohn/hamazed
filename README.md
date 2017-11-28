# What is it?

A terminal ascii game I write - to practice writing Haskell. While writing it,
several times it felt like the code I was writing could have been made simpler
and more flexible using abstractions or functional notions which I didn't know of yet
(especially for the part on Animations and how to chain them).
So if you happen to read the code and find places where it would make sense to refactor
don't hesitate to tell me, by opening an issue or submitting a merge request!

The game can be somewhat configured by the user (size and kind of the world) and
has 12 levels of increasing difficulty. The aim is to reach a given sum by shooting
at moving numbers.

The keyboard controls are mapped this way:
- ship acceleration : 's' 'e' 'd' 'f'
- laser shots       : 'j' 'i' 'k' 'l'

During the 5 first seconds of each level, the ship is immune to collisions with numbers.

# Supported Platforms / Terminals:

This is the combinations that I've tested sofar:

|OS       |Terminal      |Support|
|---------|--------------|-------|
|OS X     |Terminal      |Yes    |
|Windows  |cmd.exe       |No (due to https://ghc.haskell.org/trac/ghc/ticket/7353) |
|Linux    |GNOME Terminal|Yes    |

# Version history
- 2.0 :
  - make world configurable (square or rectangle, with or without random walls)
  - add explosion animations
  - optimize rendering using delta rendering
- 1.0 :
  - The world is a square. (Note : ship acceleration was 'w' 'a' 's' 'd' at that time)

# Build

You can build and run using `stack`:

`stack build --pedantic && stack exec hamazed-exe`

# Credits

## Delta rendering

The delta rendering code is based on code written by Rafael Ibraim:
https://gist.github.com/ibraimgm/40e307d70feeb4f117cd

It avoids to have to clear the console and redraw everything at each frame,
instead only the parts of the console that have changed are rendered,
removing unwanted flickering side effects.
