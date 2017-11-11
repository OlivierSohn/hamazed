# What is it?

A terminal ascii game I write to practice some Haskell!

# Glossary (of terms used in the backlog)

A "motion step" is a temporal duration during which the positions are fixed.
The corresponding period is a fixed number called "motion update period"

# Backlog

- ship is destroyed if touched by a number
- laser shoots only the first letter
- destroy animation

- it could be worth calling hPutStr once per frame (using a buffer in RenderState)
- try BlockBuffering (Just 80000) to reduce flicker
- laser has limited reach

- when ship and number go in opposite direction and ship shoots at each iteration
it can miss the number. Should we fix that?
- the balls that go faster should be drawn more frequently

- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
