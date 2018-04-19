- mutableByteArrayContents returns a pointer.

TODO allocate one block of memory, make sure different workers do not share cachelines.

- see if
(12,24), 4 component, 0.5 wall. | Best mean: 9'374'925 (us)
can be faster when graph memory is preallocated (next to matrix memory?)

or profile (we didn't do any profiling for multi component search yet)

- refining stage is the longest now (98%).

We could use heuristics to filter tested strategies:
the simplest one could be to just take the one we had with the first seed group and refine it with other seed groups.

tag the existing results : refined with all strategies
tag new results : unrefined

the downside is that the hints are a bit less precise.

- use an acyclic graph for difficulties, to replace proba inner list sorting + 'shouldTest'

-
Once we have all durations for (6,12), we can support user probabilities in master officially

when parameters lead to a world that has no known duration (for at least one level?),
silently change probability and/or number of components to the nearest world that has a known creation duration.
Display a message in the chat for this.

- display estimated time to build a world (based on optimal strategy time)

- the game is fun to play with all maxed:
  wall size = 6
  proba = 0.9

- parallel stats :
the stats displayed are the stats of the rng that found a world.
Should we merge them with the stats of RNGs that didn't find?

- benchmark:
instead of using asyncs, use forkIO and IORef Bool signaling when it should stop.
And when the consumer stops, it should putMVar Nothing (or Stats) to unblock the thread waiting for the result.
(On server cancelation, or on timeout, the IORef Bool is set to False.)

- benchmark IntSet vs. Set Int for Sums

- Server doesn't need to forward the request, the client continues by itself.
This will avoid unproductive waits.

- not sure if translations could be used in place of rotations (in a +1 rotation, the
  last element of the last column becomes the first element of the first column, maybe that's a problem
  as it breaks the topology in two ways (that's one too much!).)

- benchmark alternative variations:
  - rotate around 0,0, then diagonally

-
It looks like:
  For high density BIG   worlds, rotations (the way we do them today) are less usefull than random (maybe a more subtle way would be ok)
  For high density small worlds, rotations (the way we do them today) are MORE usefull than random

Maybe for big worlds, interleaving / rotating is not usefull because
  random matrices produce more randomness than interleavings or rotations
  and the cost of generating random matrices becomes negligible vs. the cost of graph creation and analysis,
  so generating only random matrices becomes a competitive strategy again.
The ratio "time to analyze a matrix vs time to generate a random matrix" could be key
  in determining the strategy to use.

Before generalizing, we should see if improvements vary with number of components / probability / size.

- the server should not start the game until all levels are possible to create.
- Create levels in the background during setup (maybe ask different nodes to compute different
levels? the bigger ones are more expensive...) and start the game once all levels are created

- Add music :
  slow (ternary) :
  1st voice:
  do . .
  | . sol
  ré - -
  - mib fa
  sol mib do
  ré . vsol
  do . .
  sol mib v si (alternate with variations :
    sol fa mib,
    sol lab sol,
    fa mi réb,
    ré . v sol,
    ré mib ré,
    sol sol sol,
    ré sol vsol)

  2nd voice:
  . . sol
  . . .
  . . sol
  . . .
  mib . .
  fa . .
  mib . .
  ré . .

  fast: (when game goes faster)
  1st voice:

  do ré mib fa
  sol . do ré
  mib fa sol .
  do ré mib fa
  ré - - -
  - - - -
  lab . . .
  sol . . .

  ^do sib la sol
  . . ^do sib
  la sol . .
  do ré mi fa
  sol - - -
  fa - - -
  mib - - -
  v sol . . .

  2nd voice:
  mib fa sol lab
  sol . sol lab
  sol fa sol .
  sol fa mib fa
  . . sol .
  v sol . sol .
  . . sol .
  mib . ré .

- while searching for a world, display this status message:

Randomly generating level 1 with constraints:
  walls size  = 6
  walls proba = 0.7
  1 connected component(s)
Number of computing nodes: 1 (i.e length of assignees)

World generation started 4 seconds ago, using random matrices, with columns and
row shuffling and matrix rotation.
12345 random matrices were generated
21312354 interleaved matrices were generated
1235416253 rotated matrices were generated
32149 Worlds were tested.
2212 were rejected due to fronteer issues.
The other have the following connected component distribution:
CC 2 .
CC 3 ...

- bug : when client reconnects, it is not reflected in the name until world is there.

- fix response on collision:
 Z
  XZ  <- here we stay one time too long.
 X

- with careful benchmarking:
** in produceUsefulInterleavedVariations : are matrices generated one by one?

** adjust the gap of number of cc in 'tryRotationsIfAlmostMatches' to optimize world generation time.

** maybe reduce number of rotations:
rotate (of 1) w times
then
rotate (of w) h times
assuming this would cover most probable cases where n connected components can join (?)

- document different permutation strategies:

Generating random numbers is expensive, hence we need permutation strategies where with a minimal amount
of permutation we can generate a big variety of number of cc, so that permutating
  becomes interesting vs generating fresh random numbers.

measure span of number of connected components, over number of permutations used.

- optimize world creation with .7 ratio:
  try 02
  generate 4 numbers (8 bit precision) per random Int using bit shifts
  replace Material by Int + constants
  parallellism: see how to set capabilities, and how to race between cores.

- optimize opengl rendering under heavy conditions (no delta rendering, a lot of successive renders,
  like in the resize scenario)

1 : optimize arithmetic operations
1.1 : precompute pixel values : type PixelValue = Vec2 Pos

- color of number explosion should be grey if number is deactivated.

- use another text box for networking messages, to separate concerns, to
prevent messages from being replaced by game info.

- a continuous version where motions are fluid, for opengl.

- Today all ' ' are (background) colored.
We need 'transparentSpace':
  transparency (or alpha) could be encoded
    in the glyph metadata or
    in the color, and filtered by the delta renderer
    of in the type : we could have [Either Invisible (Glyph, LayeredColor)],
      drawing functions would filter Invisible, interpolations would need to handle Invisible too.

Then, use it between player name and ammo.

- if performance matters here,
put Glyph in Number and Ship.
maybe in UIRectangle, too?

- Look at core, see if color is encoded n times or 1 only. (delta Draw)

- for game font,
maybe we need to make _ higher, | smaller, etc... the idea is to not modify 0-9 a-f Z T
maybe we need to move + and - to make them be in the center.
- maybe we need 2 .ttf fonts, else modified _ would look strange in messages, and pipes too ?

- replace unboxed by Storable?

- one-click "increment / decrement r,g or b (maybe use r,g,b keys)

- animate color of external frame at each shot number (fast change to another color then slow change back to normal)
: this allows to become aware of when a number is shot.

- chat : make it easier to use
- in AppState we could have arrays of random numbers, and a particle system would pass
the index of the animated point.

Hence, either the color could rely on the random part or on the frame or both.

We need n arrays of random numbers, to avoid correlations between particles of the same particle system.
A good first driver would be to emulate the randomness of fire: With an interpolation between red and yellow,
we could use random only (not frame) to define at which frame (in the interpolation / in the cube) we are.

- use location of particlesystem point to define color.

- when cycling between two colors, we cycle on a bresenham straight line.
Instead, to have richer color variations we could understand the two extremities as
the poles of a sphere (approximated by a cube?), and cycle on all colors in great circles
alternating the great circles at each interpolation of the sphere. Or spiraling on the sphere / cube.

so we need to define interpolations between 3d points: Line

- random distribution of colors : center / radius

- inject player color /gray/ component in cycle colors:
first step:
  the hardcoded colors will only be used for their hue.

- make world end when no reachable number is available. (i.e some numbers are alive,
  but in another component)

- 3 players : 2 do the 2 expressions,
one does the operation : * or +

- the count of flying items per connected component should be proportional to
the cc size.

- ships replaced by humans walking, jumping, climbing

- Duel mode with one component, the one that has the max sum wins the level number.
levels go on until 12.

Note that the start location matters a lot in that case. Think of a way to place ships in a fair way,
with the notion of "manhattan collision free distance" to number.

- (mkLevelEssence) Specialized mode where one has + the other has -.
To make players engage, they need to feel that they are both important to win
the level. Using big flying numbers and small goal number is a way to reach this.
To compensate for the small numbers of shots needed to finish a level,
we can have a list of goal numbers (3).

to compute the target we do a random operation
on existing numbers with + and -. Use big numbers.
Show minus numbers in blue

Single player where + and - alternate at every shot number

Laser color changes : red = plus

- the other ship can double numbers (make levels where we need to double
    some numbers in order to reach the sum)
- the other ship can multiply

- TextBox should take newlines into account, to make messages like:
While parsing:
  stringparsed
Error:
  ...

- multiline algo should have an option to keep multiple spaces.

- use up/down arrow to recall last message? -> shift up shift down to change cursor position.

- autocompletion based on beginning of string, right arrow to validate.
if input is a /, write all commands in help

- Press H to show help / press H to hide help (write on the right of the game)

- ranking :
high scores of a user can be stored locally, and sent to other clients when connecting to a server.
How to prevent fraud?

- identify clients to match on reconnect (ip + player name ?)

- recover on disconnections (intentional or not), client-side:
    when server connection vanishes, client could say:
      "Server is not reachable, please wait..."
    Then, when server is up again, (client trying every second),
    the client sends, along with the Connection request, its GameStateEssence + Level
    so that the server creates the corresponding CurrentGame, and ServerState.

- server transfer:
  During the game, we could elect a new server when the server dies.
  When the server shutdowns, we could let the clients live, so that they can keep the game state.

- when in Setup intent, the server should send updates of a list "which players would be in the
game, should you hit Space now" to the clients

- when player presses a key to restart: it waits for other player to also press a key to restart
we could display "waiting for other players to press a key..."

- the info "game ended" is sent when player presses play.
then server verifies outcome and computes next world.

Instead, the info should be sent immediately so that the server can verify
outcomes.

- when changing view center, the text on the left does not change position, it
changes only on the next world change.

- investigate using higher level concepts to clarify server / client statefull interactions
and make the implementation more robust, to be confident that all edge cases are well-handled:
describe logic with a GRAFCET.

- broadcast every player state change, to have the following UI:
                                                        - Players:
                                                            Player1 [Level 3 running]
                                                            Player2 [Level 3 won]
                                                            Player3 [Waiting]

                                                        - Chat:
                                                        (Player 1) Hello !
                                                        (Player 2) Salut
                                                        <Player 3 joined>
                                                        (Player 2 typing...)
                                                        > Comm|

- I dropped the changes in time period (accelerating during game, resetting on collision
  with initalGameMultiplicator). Should we use it?

- now that we have multiple threads, revise number of capabilities

- if the server closes the connection, the next send from client should fail :
can we automatically reconnect in that case? All peers should store their last GameStepIdx
to know which was more advanced, and start from that state.
- on connection, if server is not found, retry every 3 seconds.

- when talking, animate text.

- use bitwise package to optimize boolean matrix representation.

- try round instead of square for walls.

- go back and forth in time.

- store update time in deadlines, to replace "while we are under the budget"
  by "if curTimeSpent + foreseenTimeSpent < budget then update this deadline"
  - Note that this cost varies : for particle systems, full-grown particles take
  more time to update / game moved takes longer if there is a collision / laser shot
  takes longer if there is a collision

- try glfw on windows, if it works, disable limitation and force the use of glfw on windows

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

Tools:
- analyse using https://hackage.haskell.org/package/threadscope
https://making.pusher.com/latency-working-set-ghc-gc-pick-two/
