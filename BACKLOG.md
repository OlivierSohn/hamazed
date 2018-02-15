
- graceful shutdown with "Ctrl + C" :
http://zguide.zeromq.org/hs:interrupt
also see https://github.com/jaspervdj/websockets/issues/135 (last comment)

should fix : when player 2 connects, then ctrl c then connects, it exits with "disconnected by Server"

- fix UI: the name of players is far away to the left, not very visible is window is not big enough.
- fix display of level (when a player joins)

- recoverability:
    on disconnections (intentional or not):
      server-side:
        pause game when a client is not reachable, and broadcast:
          "Playerx is not reachable, retrying in 3 seconds"
      client-side:
        when server connection vanishes, client could say:
          "Server is not reachable, retrying in 3 seconds"

- when Excluded, the client should display :
  "A game is in progress on this server, you will join once the game ends. Please wait..."
  and the player should be able to use the chat.
- when in Setup intent, the server should send updates of a list "which players would be in the
game, should you hit Space now" to the clients

- client-less server mode
Pro : garbage collection of client does not influence server garbage collection, so
game scheduling may become more stable because on the server there is less stuff to collect.
(provided that if 2 haskell processes run on the same system, they don't share the runtime)
Also, it allows to have a dedicated game server.

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
