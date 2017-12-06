# What is it?

A terminal ascii game I write - to practice writing Haskell. In this game, you
fly a ship in a space full of flying numbers. You can shoot at the numbers using
the ship's laser, and once the numbers you shot sum up to the level's objective number,
you go to the next level. Higher levels have more numbers, and less space! The last
level is level 12. I never won this game, but I hope somebody will (without changing the code :)).

The game can be configured in "world shape" (square, rectangle) and "kind of walls"
(none, deterministic, random).

You can define your own keyboard mapping by modifying the 'eventFromChar' function in src/Game/Event.hs,
the default mapping is:
- ship acceleration : 's' 'e' 'd' 'f'
- laser shots       : 'j' 'i' 'k' 'l'

Oh, and during the 5 first seconds of each level, the ship is immune to collisions with numbers.

# Supported Platforms / Terminals:

|OS       |Support|
|---------|-------|
|OS X     |Yes    |
|Linux    |Yes    |
|Windows  |No, due to https://ghc.haskell.org/trac/ghc/ticket/7353 |

For best results, the terminal should support 8-bit colors and not redefine system colors.

# Version history
- 2.1 :
  - New animations :
    - In colors (8-bit)
    - Also between levels
    - Using physics and gravity
    - "Terminal size"-aware
- 2.0 :
  - World is configurable (square or rectangle, with or without random walls)
  - Explosion animations
  - Optimized rendering (delta rendering)
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
