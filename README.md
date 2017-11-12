# What is it?

A terminal ascii game I write to practice some Haskell! It has 12 levels, and I reached level 9 once.

# Backlog

- when lose, do not stop animations, stop only move.
- make laser ray render persist until next motion step.
- do not count duplicate laser shots in same motion step.
- make destroy animations in the background with a faster refresh rate than the rest:
  - gravity based (parabollas)
  - an expanding circle

To do that we will need to add an Action "AnimationTimeout"

- make target more visible

- write a help : during the 5 first seconds, the ship is immune to collisions
- current implementation is easy mode, make a medium mode where the
numbers of lasers available is different for each direction
and a hard mode where there is a timeout on each level.

Also change the motion period for various difficulty levels

- it could be worth calling hPutStr once per frame (using a buffer in RenderState)
- try BlockBuffering (Just 80000) to reduce flicker

- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
