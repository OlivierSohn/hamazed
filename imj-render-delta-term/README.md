# What is it?

A terminal render engine that mitigates screen tearing when rendering complex
game graphics, by minimizing the amount of data sent to stdout.

# Projects using it

- [Hamazed, a game with flying numbers](https://github.com/OlivierSohn/hamazed/tree/master/imj-game-hamazed)

# Supported Platforms / Terminals:

|OS       |Support|
|---------|-------|
|OS X     |Yes    |
|Linux    |Yes    |
|Windows  |Yes    |

# Version history

Not released yet

# Build

You can build using [stack](https://docs.haskellstack.org):

`stack build --pedantic`

# Credits

- Rafael Ibraim : [Base implementation for delta rendering](https://gist.github.com/ibraimgm/40e307d70feeb4f117cd)

- Andras Kovacs : [Base implementation for the C++/STL style vector in Render.Delta.DynUnboxedVec](https://github.com/AndrasKovacs/dynamic-mvector).
