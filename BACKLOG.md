
- deadline updates.

Today the loop is:
  - update soonest or most prioritary deadline
    render

As a first step:
  - let timeBudgetForUpdate = 0.008 seconds
        not_too_far = 0.01 seconds
    update deadline while
      we are under the budget, and
        deadline is overdue
        or deadline is not_too_far from now
    -- the order of updates is by priorities for overdue deadlines, like today
    render

As a second step:
  - let loopBudget = 0.01 seconds
        timeBudgetForUpdate = loopBudget - max time to render

As a 3rd step:
  - store update time in deadlines, to replace "while we are under the budget"
    by "if curTimeSpent + foreseenTimeSpent < budget then update this deadline"
  - Note that this cost varies : for particle systems, full-grown particles take
  more time to update / game moved takes longer if there is a collision / laser shot
  takes longer if there is a collision

As a 4th step, to resolve the tension between:
  * doing less rendering steps because rendering takes time:
    - writing everything to the back buffer
    - computing differences
    - sorting them / rendering them
  * (for terminal rendering) doing more rendering steps to not overflow stdout.
  - update deadline only if time to render + foreseen time to update deadline < deadline time
Hence, "when we can afford it", we keep a high number of rendering steps, but if
it is at the cost of making some animation deadlines overdue, we do
less rendering steps instead.

- try glfw on windows, if it works, disable limitation
- try retina glfw

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
