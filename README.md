# What is it?

It's a terminal ascii game where you fly a ship through numbers. The goal is to shoot
the numbers whose sum will be equal to the level's target. It's easy at the beginning,
but higher levels have more and more numbers, and less and less space to navigate through them!
The last level is level 12: I never reached it, but I hope somebody will :).

# Demos

## No walls, square world

[![asciicast](https://asciinema.org/a/151434.png)](https://asciinema.org/a/151434)

## Random walls, rectangular world

[![asciicast](https://asciinema.org/a/uYy7GU9Uzs68PF102Cfsx1olY.png)](https://asciinema.org/a/uYy7GU9Uzs68PF102Cfsx1olY)

# Configurability

The game can be configured in "world shape" (square, rectangle) and "kind of walls"
(none, deterministic, random).

![Configuration snapshot](images/config.png?raw=true "Configuration")

You can define your own keyboard mapping by modifying the 'eventFromChar' function
defined [here](src/Game/Event.hs), the default mapping being:
- ship acceleration : 's' 'e' 'd' 'f'
- laser shots       : 'j' 'i' 'k' 'l'

# Supported Platforms / Terminals:

|OS       |Support|
|---------|-------|
|OS X     |Yes    |
|Linux    |Yes    |
|Windows  |No (see [this](https://ghc.haskell.org/trac/ghc/ticket/7353)) |

Your terminal window should have a dimension of at least {height = 42, width = 146}.
If it is too small, the program will fail with the following error message:

```
From game thread:

Minimum terminal size : Window {height = 42, width = 146}.
Current terminal size : Window {height = 22, width = 165}.
The current terminal size doesn't match the minimum size,
please adjust your terminal size and restart the executable.
```

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

You can build and run using [stack](https://docs.haskellstack.org):

`stack build --pedantic && stack exec hamazed-exe`

# Credits

## Delta rendering

The initial idea for delta rendering is based on [code written by Rafael Ibraim](https://gist.github.com/ibraimgm/40e307d70feeb4f117cd)
