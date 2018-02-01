- multiplayer mode where two ships (one cannot collide the other) work on the same sum.
If one ship collides, the other can continue.
  - the other ship can double numbers (think of levels where we need to double
    some numbers in order to reach the sum)
  - the other ship can multiply

  - one of the entities will be the server, containing game/world state and making the world step.
  clients know when the server will send an update (todo explain how) event hence
  if there is a connection problem, they can say so (lost connection, please wait...)

  0th step : use multi cast with a single user group : write to the group, and read from the group.

  1st step: find a way to communicate between client and server.
  compare using :
  - http://haskell-distributed.github.io/
  - https://hackage.haskell.org/package/network-multicast

- make font characters more square

- make distance between world and text depend on unit width

- try round instead of square for walls.

- go back and forth in time.

- add state transitions:

  - hitting escape:

  play -> quit the current game? yes / no -> configuration -> quit ? yes / no

  - hitting space : play -> paused -> play

should animations continue or should we pause them?
if yes we should have a time offset in the state.

- display debug infos in a nice way when using the terminal :
overlay,

- store update time in deadlines, to replace "while we are under the budget"
  by "if curTimeSpent + foreseenTimeSpent < budget then update this deadline"
  - Note that this cost varies : for particle systems, full-grown particles take
  more time to update / game moved takes longer if there is a collision / laser shot
  takes longer if there is a collision

- try glfw on windows, if it works, disable limitation

- use http://book.realworldhaskell.org/read/monad-transformers.html
to simplify if needed

- a game where we need to escape a randomly generated maze.

- in renderer, add the possibility to just change a color (fg or bg), just change a char.
  - use it to create end level effects : draw everything with '.' or '+' except the winning message
  and the messages on the left.
  - when laser is shot, make a circle expanding, where chars are overridden by '+' or
  color is overridden.
- cross-dissolve between worlds, to have smooth transitions (especially important
  when there are walls)

- when frame morphs, it should be centered in the screen, hence:
use an evolution to replace the ship in the center, morph, then use another
evolution to place the ship where it is in the next world.

- scissors should be combinable : we need a stack of scissors.

- Measure if O2 is necessary, especially for:
  - particle systems with a lot of points
  - delta renderer
compilation times are slower by 4% with -O2.

- for stackage, which ghc versions should I support? https://www.fpcomplete.com/blog/2014/05/stackage-server
