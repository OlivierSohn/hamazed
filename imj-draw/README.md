# What is it?

A 2d-terminal game engine with:
- Discrete geometry
- Animations, chained (or composed) animations
  - Gravity-based
  - Explosive
  - Lasers
- Color animations (interpolated colors)
- Text animations (location and color)
- Rendering class : Draw, implemented in package "delta-render" to draw efficiently
  in the console without screentearing.

# Projects using it

- [Hamazed, a game with flying numbers](https://github.com/OlivierSohn/hamazed)

# Supported Platforms / Terminals:

|OS       |Support|
|---------|-------|
|OS X     |Yes    |
|Linux    |Yes    |
|Windows  |Yes    |


# Version history

- Not released yet

# Build

You can build using [stack](https://docs.haskellstack.org):

`stack build --pedantic`

# Contributions

Contributions are welcome, make sure you can build with `stack build --pedantic`
before submitting a pull request.
