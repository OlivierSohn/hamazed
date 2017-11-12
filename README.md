# What is it?

A terminal ascii game I write to practice some Haskell! I reached level 8 once :)

# Backlog

- number of lasers available is different for each direction
- when ship and number go in opposite direction and ship shoots at each iteration
it can miss the number. To fix that we can require the ship to have speed .5 at max
- if letters are shot fast enough, get a bonus (auto shoot)
- destroy animation

- it could be worth calling hPutStr once per frame (using a buffer in RenderState)
- try BlockBuffering (Just 80000) to reduce flicker

- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
