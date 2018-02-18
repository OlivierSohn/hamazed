- "Please Wait" after level finished can be long if other player doesn't press the key,
we could inform by state: GameState WaitingAcknowledgement [ShipId]
- battle mode where one has + other has -, and the goal is to have a final sum of the sign of the ship.
maybe in a finite time, with new numbers appearing regularily.

- ranking :
high scores of a user can be stored locally, and sent to other clients when connecting to a server.
How to prevent fraud?

- in case All players were disconnected from the server, we would need the clients to say
"I have a game state of ..." and if other players agree with the Id/state we can continue the game.

- when a client connects we could first ask the gamestate and its shipId :
  the client responds Nothing if worldId is Nothing.

- recover:
    on disconnections (intentional or not):
      server-side:
        if a playing client is not reachable, broadcast :
          pause game due to disconnection of client x.
            Then client displays "Game paused due to lost connection to player x, please wait..."
          The client can reconnect (the server stores its IP and when we detect it is back,
           we can re-integrate it in the game by sending the world state from another playing client.
          Then broadcast "resume game"
      client-side:
        when server connection vanishes, client could say:
          "Server is not reachable, please wait..."

If we detect that the connection is ko (either while sending a game update or while reading or in the pingpong thread)
, when connection is re-established, one client should transmit the full state of the world.
 (worldessence). To do that we should refactor and have a single world in the state.

- What if the connection just becomes extremely slow? At what point should we consider that the player can't play anymore?
Display roundtrip times below player names.
Client could have an endpoint and thread dedicated to responding to ping.

- During the game, write (disconnected) next to a player's name that was disconnected.
  Then when back to setup, do not mention the disconnected player.
- fix UI: the name of players is far away to the left, not very visible is window is not big enough.

- graceful shutdown with "Ctrl + C" for windows : http://hope.simons-rock.edu/~pshields/cs/cmpt312/libraries/base/GHC-ConsoleHandler.html

- when Excluded, the client should display :
  "A game is in progress on this server, you will join once the game ends. Please wait..."
  and the player should be able to use the chat.
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

- multiplayer mode where two ships (one cannot collide the other) work on the same sum.
If one ship collides, the other can continue.
  - the other ship can double numbers (think of levels where we need to double
    some numbers in order to reach the sum)
  - the other ship can multiply

- use bitwise package to optimize boolean matrix representation.

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

Tools:
- analyse using https://hackage.haskell.org/package/threadscope
https://making.pusher.com/latency-working-set-ghc-gc-pick-two/
