- Use memory pools for :
* channels
* instruments.

https://github.com/foonathan/memory/blob/master/include/foonathan/memory/memory_pool.hpp

The user of the library is free to specify the initial pool sizes, in number of concurrent instruments?

Garbage collection should occur
* when a new instrument is created

maintain a global list (under lock) of lambdas pairs:
  - one to know if the synth is playing :
      checks if orchestrators and computes are empty, and
        if all mononotechannel elements enveloppes are done.
      the function should take the isUsed lock of the instrument else race conditions will occur.
  - one to finalize the synth (and hence return the channels) : after calling this we remove the pair.

- make a player app where a melody is played and we can interactively change the instrument used to play it.

- Decay :
    Ease Out Circ ++

long Attack + release (Ease InOut Sine), Sustain 1 : musical saw

long attack, EaseIn Sine : smooth

- make volume of music / effects adjustable in hamazed

- limit the sustain value to 0.01 when using propder for decay
- at each param change, play a note?
- read/use http://werner.yellowcouch.org/Papers/fastenv12/

- Ideally we should JIT the parametrized audioelements code to have better performance: http://www.stephendiehl.com/llvm/

- use 64 bit audio to reduce numerical errors (especially when summing a big signal with a small one)
- remove 256 channels limit (review types for channel ids : uint8_t -> int, use -1 or maxBound for None)

- on soundengine, instead of using the channel xfade,
the duration could be interpreted by the channel as "when to trigger the enveloppe keyReleased()"
(thus, also when to trigger the enveloppe of the following request).

- use travis_wait 119 if haddock is too long

- develop enveloppes:

make "linear perceived" envelope : Logarithmic, with a negative offset, scaled:.
* scale:
    epsilon -> 1
    0       -> 1

use leap motion as input for envelope / pitch?

- synths : verify if with 2 players, ghost notes occur.

- https://devcenter.heroku.com/changelog-items/1418 -- take it into account in doc

- add game doc., with /man /doc commands
For synth:
Use the computer keyboard to play notes.

Played notes are stored in a Recording:
* Pressing 'Space' will start a new singleline Sequence from the current Recording.
* Pressing one of the function keys fn where n is in [1..4] will:
  if multiline Sequence n doesn't already exist : start a new sequence using the current Recording.
    The period of the sequence is the length of the current Recording.
  if multiline Sequence n exists : insert the current Recording in the sequence.
    If the current Recording is longer than the existing sequence, multiple instances of it will be played at the same time.
    Note that this could lead to sound saturation if the Sequence period is short w.r.t the length of the Recording.
* Pressing 'f10' empties the current Recording.

- size of UI should adapt:
- Display sequences in UI, vertically:

          Sequence 0                  Sequence 1
    ............|..........     ............|..........     -- the progress of the sequence

1: -*-*--*-*-*--*-*--*-*-*-   3:-*-*--*-*-*--*-*--*-*-*-
2: -*-*--*-*-*--*-*--*-*-*-   4:-*-*--*-*-*--*-*--*-*-*-

- the ability to delete a loop / mute it
- add command help

- parallelize player input reading:
```user
readInput = fmap (Right . dispatch) (readTQueue server)
        <|> fmap Left (readTQueue platform)
```
if the server event cannot be handled because of an exclusivity key conflict, we should handle
platform events instead.

Both streams of events (server events and platform events) are guaranteed to be handled
by order of arrival in their respective queues.

We could let the programmer decide if we break that guarantee for audio events
, and let them be handled before being put in the queue, to have a better timing precision:

  - In case of an audio app, we want the audio timing to be very accurate.
The graphics timing can be less accurate.
  - In case of a game, we want audio to be synchronized with game events
so handling audio events the same way as game event might be preferable.

- array frame colors (grey for synths)

- make generic : the server sends the game state to the client (putIGame / withAnim)

- can `notifyClient' $ EnterState $ Included $ PlayLevel Running` in `clientCanJoin` be made generic?

- OnContent is not handled generically.
maybe content should not be generic at all.
Re-evaluate pros/cons for that.

pro : Instructions for setup drawing in the viewport.

- make a version where we can only move at all times, and not go diagonally.

- using withAnim is tricky: we need to use it
for every state action that may result in a changed 'getClientsInfos', 'getViewport' or 'mkWorldInfos'.

We should come up with a better design where the library user doesn't have to care about these aspects.
For example, on every action run in the state monad, we could use it and run the animation only if something
changed.

- Joining... should not appear in tutorial
we could disable the behavior where before displaying a new string, we display the other one in full.
Instead, continue the animation to its end and then start the other one, or reverse the animation.

- Document why we don't use a deadline technique for server similar to the client (maybe its justified, but I'm not sure)

- Wait for all players to press a key before starting the game.

- imj-server could depend on a smaller version of imj-base

- (while looking at imj-server dependencies):
GameItem should be moved out of imj-base, it is too game-specific.

- make music evolve with the numbers shot in the game.

- use 'Communication' sound while computing the world.

- laser should have a sound, maybe depending on the music, it could be a chord.
- winning or losing should change the music

- when the client is computing the world, we could make audioout sleep to have more CPU available.

- on client termination, sound should close gracefully, i.e fade out.
- add a keyboard so that players can play music collaboratively:
the note should transit through the server before being played so that both players have the latency.

- animate on rebound on Z wall : wave
on frame: ?

- update readme demos with new UI: add a demo with 2 players.

- make a standalone library for generating rectangle binary random (small) worlds.
(move tests of topology there)

- benchmark:
instead of using asyncs, use forkIO and IORef Bool signaling when it should stop.
(On server cancelation, or on timeout, the IORef Bool is atomically set to False.)
When the consumer reads False, it putMVar Nothing (or Stats) to unblock the thread waiting for the result.

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

- one-click "increment / decrement r,g or b (maybe use r,g,b keys)

- animate color of external frame at each shot number (fast change to another color then slow change back to normal)
: this allows to become aware of when a number is shot.

- chat : make it easier to use
- in (AppState s) we could have arrays of random numbers, and a particle system would pass
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
