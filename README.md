# What is it?

A terminal ascii game I write to practice some Haskell!

# Backlog

- handle repeated keys by not making the game fast forward
-- use a timeout of 0 for getChar, loop until timeout is reached

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
