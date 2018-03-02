- command line arg for rectangle unit size (Client - OpenGL)
- command line arg for screen size
- command line arg for full screen -> adapt the logic because we can't guarantee that the screen size
 will be a multiple of unit size.

- remove 'Info|' logs when running in the terminal?

- Today all ' ' are (background) colored.
We need 'transparentSpace':
  transparency (or alpha) could be encoded
    in the glyph metadata or
    in the color, and filtered by the delta renderer
    of in the type : we could have [Either Invisible (Glyph, LayeredColor)],
      drawing functions would filter Invisible, interpolations would need to handle Invisible too...

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

- use 'sameIntensityHues' where appropriate.

- make stats of world dismissals visible to the user

- make world end when no reachable number is available. (i.e some numbers are alive,
  but in another component)

- 3 players : 2 do the 2 expressions,
one does the operation : * or +

- the count of flying items per connected component should be proportional to
the cc size.

- ships replaced by humans walking, jumping, climbing

- when making level essence, we should know the number of players.

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

- notify on successful commands: "'Player' changed its name to 'Me'"

- multiline algo should have an option to keep multiple spaces.

- use up/down arrow to recall last message? -> shift up shift down to change cursor position.

- autocompletion based on beginning of string, right arrow to validate.
if input is a /, write all commands in help

- Press H to show help / press H to hide help (write on the right of the game)

- When server unreachable, sendToServer Disconnect does nothing, so Escape key doesn't work as intended.
- Message displayed when server is unreachable is wrong (a game is currently running on the server)
- "Please Wait" after level finished can be long if other player doesn't press the key,
we could inform by state: GameState WaitingAcknowledgement [ShipId]

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
  During the game, we could transfer the server (either because the game server died,
    or because it's fun to do). To do that, each client needs all the info
    to create an accurate game state.
  When the server shutdowns, we could let the clients live, so that they can keep the game state.

- fix UI: the name of players is far away to the left, not very visible is window is not big enough.

- graceful shutdown with "Ctrl + C" for windows : http://hope.simons-rock.edu/~pshields/cs/cmpt312/libraries/base/GHC-ConsoleHandler.html

- when in Setup intent, the server should send updates of a list "which players would be in the
game, should you hit Space now" to the clients

- client-less server mode
(verify that ctrl c terminates the process gracefully)
Pro : garbage collection of client does not influence server garbage collection, so
game scheduling may become more stable because on the server there is less stuff to collect.
(provided that if 2 haskell processes run on the same system, they don't share the runtime)
Pro : it allows to have a dedicated game server.
Pro : we can install a signal handler on the client (the one installed today is dedicated to the server)
Pro : we can play in the terminal, and log in the console for the server.
Cons : when running the game we need to start the server, then start the client.

- using port 80 to listen is not allowed?

- when handling connection closed, we putStrLn, this is problematic when player uses terminal rendering.
We could comment that for now, and rely on chat messages sent by the server before the disconnection
to be aware of disconnections.

- when player press a key to restart: it waits for other player to also press a key to restart
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

- display debug infos in a nice way when using the terminal :
overlay,

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
