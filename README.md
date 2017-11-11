# What is it?

A terminal ascii game I write to practice some Haskell!

# Glossary (of terms used in the backlog)

A "motion step" is a temporal duration during which the positions are fixed.
The corresponding period is a fixed number called "motion update period"

# Backlog

- do not collide for the first 5 seconds to give time for the user to be in the game
- support A to F
- number of lasers available is different for each direction
- when ship and number go in opposite direction and ship shoots at each iteration
it can miss the number. To fix that we can require the ship to have speed .5 at max
- letters must be shot in a specific order
- if letters are shot fast enough, get a bonus (auto shoot)
- destroy animation

- it could be worth calling hPutStr once per frame (using a buffer in RenderState)
- try BlockBuffering (Just 80000) to reduce flicker
- laser has limited reach
- the balls that go faster should be drawn more frequently

- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
