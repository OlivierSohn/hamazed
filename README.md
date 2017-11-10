# What is it?

A terminal ascii game I write to practice some Haskell!

# Glossary (of terms used in the backlog)

A "motion step" is a temporal duration during which the positions are fixed.
The corresponding period is a fixed number called "motion update period"

# Backlog


- allow multiple actions within a single motion step
- if multiple laser shots happen, render at exact time when it occurs
-- render everything


- try BlockBuffering (Just 80000) to reduce flicker
- destroy animation
- ship is destroyed if touched by a number
- laser shoots only the first thing
- laser has limited reach

- when ship and number go in opposite direction and ship shoots at each iteration
it can miss the number. Should we fix that?
- the balls that go faster should be drawn more frequently

- make a brick-breaking game
- make a tower based defense game
- make a pong

- Play multiple games at once
