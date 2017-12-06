# What is it?

A terminal ascii game where you fly a ship through numbers. The goal is to shoot
the numbers, using the ship's laser, whose sum will be equal to the
level's target. It's easy at the beginning, but higher levels have more and more numbers,
and less and less space to navigate through them! The last
level is level 12. I never reached it, but I hope somebody will :).

The game can be configured in "world shape" (square, rectangle) and "kind of walls"
(none, deterministic, random).

You can define your own keyboard mapping by modifying the 'eventFromChar' function in src/Game/Event.hs,
the default mapping is:
- ship acceleration : 's' 'e' 'd' 'f'
- laser shots       : 'j' 'i' 'k' 'l'

And during the 5 first seconds of each level, the ship is immune to collisions with numbers.

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
    - With colors (8-bit)
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
