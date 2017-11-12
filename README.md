# What is it?

A terminal ascii game I write to practice some Haskell! It has 12 levels, and I reached level 9 once.

# Backlog

- destroy animation
- make target more visible

- write a help
- current implementation is easy mode, make a medium mode where the
numbers of lasers available is different for each direction
and a hard mode where there is a timeout on each level

- it could be worth calling hPutStr once per frame (using a buffer in RenderState)
- try BlockBuffering (Just 80000) to reduce flicker

- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
