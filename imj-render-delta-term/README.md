# What is it?

A terminal render engine that mitigates screen tearing when rendering complex
game graphics, by minimizing the amount of data sent to stdout.

# Projects using it

- [Hamazed, a game with flying numbers](https://github.com/OlivierSohn/hamazed)

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

- The initial idea for delta rendering is based on [code written by Rafael Ibraim](https://gist.github.com/ibraimgm/40e307d70feeb4f117cd)

- The C++/STL style vector in Render.Delta.DynUnboxedVec is modified from
<https://github.com/AndrasKovacs/dynamic-mvector dynamic-mvector by Andras Kovacs>.
