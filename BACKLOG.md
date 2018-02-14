- see if using port 80 to listen works.

- use separate processes for server and client?
Pro : garbage collection of client does not influence server garbage collection, so
game scheduling may become more stable because on the server there is less stuff to collect.

- fix display of level

- add player name in front of corresponding ammo

- when player press a key to restart: it waits for other player to also press a key to restart
we could display "waiting for other players to press a key..."

- the info "game ended" is sent when player presses play.
then server verifies outcome and computes next world.

Instead, the info should be sent immediately so that the server can verify
outcomes.

- when server is not reachable, output an error.

- when changing view center, the text on the left does not change position, it
changes only on the next world change.

- handle exceptions of receiveDataMessage:

-- | Receive an application message. Automatically respond to control messages.
--
-- When the peer sends a close control message, an exception of type 'CloseRequest'
-- is thrown.  The peer can send a close control message either to initiate a
-- close or in response to a close message we have sent to the peer.  In either
-- case the 'CloseRequest' exception will be thrown.  The RFC specifies that
-- the server is responsible for closing the TCP connection, which should happen
-- after receiving the 'CloseRequest' exception from this function.
--
-- This will throw 'ConnectionClosed' if the TCP connection dies unexpectedly.
receiveDataMessage :: Connection -> IO DataMessage

- replace DisconnectionAccepted by using 'sendClose' to close connections:

sendClose :: WebSocketsData a => Connection -> a -> IO () Source#

Send a friendly close message. Note that after sending this message, you should still continue calling receiveDataMessage to process any in-flight messages. The peer will eventually respond with a close control message of its own which will cause receiveDataMessage to throw the CloseRequest exception. This exception is when you can finally consider the connection closed.


- investigate using higher level concepts to clarify server / client statefull interactions
and make the implementation more robust, to be confident that all edge cases are well-handled:
describe logic with a GRAFCET.

- investigate error seen in terminal, is it when closing the app? if so, can we close more gracefully,
else this is a bug.

- There are 2 types of interactions:

  - client state changes:
ServerEvent : "go to state 'X' now"
ClientEvent : "I've finished state 'X'"
(client rendering and client keymaps depend on state)

  - server requests:
ServerEvent : Create a world like this
ClientEvent : Here is the world you asked

ServerEvent : Use this world now.
ClientEvent : Ok, the world is displayed and animation between previous world
              and this world has finished.

Server stores global intent, which influences how client events are handled.

Use-case, client side 1:

(program start)
(once no other client is playing anymore)
receive ServerEvent $ EnterState Setup -> state = `Ongoing` `Setup` (defines keyboard mapping)
(when player changes a parameter)
send ClientEvent $ ChangeWallDistribution ... -> server broadcasts "clientX changed world parameter"
                                               , puts /last world request/ to "172"
                                                then asks world creator to build a world for Level1, and passes 172,
                                                which will be included in the response to be able to tell if the world corresponds.
                                                , once server received world 172:
receive ServerEvent $ ChangeLevel LevelSpec WorldEssence -> displays the world
send ClientEvent $ IsReady 172
(when player hits space)
send ClientEvent $ ExitedState Setup -> state = `Done` `Setup`
                                       , server broadcasts EnterState `Done` `Setup`
                                          because we don't want other players to change the world
                                       , server broadcasts 'player' started the game
                                          to inform other players of what happened
                                       , server puts /last world request/ to "173"
                                        then asks world creator to build a world for Level1, and passes 173,
                                        which will be included in the response to be able to tell if the world corresponds.
                                       , once server received world 173:
receive ServerEvent $ ChangeLevel !LevelSpec !WorldEssence
(once UI animations to transition level are done)
send ClientEvent $ IsReady 173
(when all clients in `curClients` sent IsReady 173)
receive ServerEvent $ EnterState `Ongoing` `PlayLevel` (defines keyboard mapping)
(when lose or win)
send ClientEvent $ ExitedState PlayLevel (Win | Lose)
(when all clients in `curClients` sent the same)
receive ServerEvent $ EnterState ...

Use-case, client side 2:
(program start)
(once no other client is playing anymore)
receive ServerEvent $ EnterState Setup -> state = `Ongoing` `Setup` (defines keyboard mapping)
receive ServerEvent $ EnterState `Done` `Setup` -> state = `Done` `Setup`

-
`curClients` = group of clients that were in "Setup" when one client hit space.

              user sees "a game is in progress, please wait..."
              and can communicate with the chat.
              |
            WaitingForServer
                   |
                   | [if all clients are in Setup]
                   |
                   |   .----------------------------------------<  Lost level
                   |   |   --------- > -----------                  ^
                   v   v /                         \                |
                   Setup -> Computing World -> Changing Level -> Playing level
                    |  ^        ^                   ^               v
display which players  |        .-------------------.-----------<  Won Level
would be in the game   |                                            v
if we hit space        .----------------------------------------<  Won Game

When a client enters "Setup", it is added to `curClients`.

on Level lose, Level win, wait for other clients to reach the same result before triggering
the next transition.


- broadcast every player state change, to have the following UI:
                                                            Player1 [Level 3 running]
                                                            Player2 [Level 3 won]
                                                            Player3 [Waiting]

                                                        (Player 1) Hello !
                                                        (Player 2) Salut
                                                        <Player 3 joined>
                                                        (Player 2 typing...)
                                                        > Comm|


- I dropped the changes in time period (accelerating during game, resetting on collision
  with initalGameMultiplicator). Should we use it?

- now that we have multiple threads, revise number of capabilities

- do not crash client when server dies without disconnecting clients.
- when quitting program, we should stop the server gracefully to avoid exceptions
in distant clients.
see https://github.com/jaspervdj/websockets/issues/135 on how to handle exceptions on the client.
- when exiting the program, should the client close the connection:
sendClose conn ("Bye!" :: Text)
then wait one second?
- if the server closes the connection, the next send from client should fail :
can we automatically reconnect in that case? All peers should store their last GameStepIdx
to know which was more advanced, and start from that state.
- on connection, if server is not found, retry every 3 seconds.

- the server creates the shipids.

- when talking, animate text.

- when hitting Esc in the console, we need to hit enter after to stop the program.
investigate...

- a client with ip "IPC" creates the world, publishes it as multiplayable.
other clients can connect to "IPC" enter it. then the game is started when all players agree to start.
When all players have finished animating the transition, the server starts it.

On every level, The client that created the world creates the next world, publishes it
(world + positions) then the server starts the transition, then when all players
have finished animating the transition, the server starts it.

- "ship safe until" is managed by the server, else results can differ between clients!

- continue the level until all ships are destroyed

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
  - http://haskell-distributed.github.io/tutorials/tutorial-NT2.html
  - https://hackage.haskell.org/package/network-multicast
  - https://wiki.haskell.org/Implement_a_chat_server

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

- for stackage, which ghc versions should I support? https://www.fpcomplete.com/blog/2014/05/stackage-server
