{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Data.Bool(bool)
import           Data.Monoid((<>))
import           Control.DeepSeq(NFData)
import           Data.Attoparsec.Text(skipSpace, atEnd, takeText)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text(unpack)
import           Data.Proxy(Proxy(..))
import           Data.Binary(Binary(..))
import           GHC.Generics(Generic)

import           Imj.Categorized
import           Imj.Game.App(runGame)
import           Imj.Game.Class
import           Imj.Game.Command
import           Imj.Game.Modify
import           Imj.Game.Status
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Screen
import           Imj.Graphics.Render.FromMonadReader(drawStr)
import           Imj.Graphics.UI.Chat
import           Imj.Graphics.UI.RectContainer
import           Imj.Input.Types
import           Imj.Server.Class
import           Imj.Server.Connection
import           Imj.Server.Types
import           Imj.Util

-- 'Proxy' is used to "tell" the type system what is the type of game:

main :: IO ()
main = runGame (Proxy :: Proxy IncGame)

newtype Counter = Counter Int
  deriving(Generic, Binary, Show, NFData, Num, Integral, Real, Ord, Eq, Enum)

-- | This implements the client-side logic of the game
data IncGame = IncGame {
  theClientCounter :: !Counter
} deriving(Show)

-- | 'IncServer' is the game-specific state of the server. It just holds the counter:
data IncServer = IncServer {
  theServerCounter :: !Counter
} deriving(Generic)
instance NFData IncServer

-- | This represents events generated by the server, sent to the clients.
data IncServerEvent =
    CounterValue !Counter
  deriving(Show,Generic)
instance Binary IncServerEvent
instance Categorized IncServerEvent
instance DrawGroupMember IncServerEvent where
  exclusivityKeys = \case
    CounterValue {} -> Set.singleton GameStep

-- | This represents events generated by the client and sent to the server.
data IncClientEvent =
    IncrementCounter
  deriving(Show,Generic)
instance Binary IncClientEvent
instance Categorized IncClientEvent

-- | A custom command handled by the server
data IncCommand = ResetCounter
  deriving(Show, Eq, Generic)
instance Binary IncCommand


-- 'evenTriangle' is used to make the size of the frame vary with the counter:
evenTriangle :: Int -> Int
evenTriangle = (*) 2 . zigzag 0 5

-- Client-side classes are prefixed by @Game@, while server-side classes are
-- prefixed by @Server@.

instance GameExternalUI IncGame where

  gameWindowTitle = const "Increment the counter!"

  -- This defines the size of the outer frame you see when running the game.
  -- Note that the size depends on the counter value:
  getViewport _ (Screen _ center) (IncGame n) =
    let i = evenTriangle $ fromIntegral n
    in mkCenteredRectContainer center $ Size (10+fromIntegral i) (10+fromIntegral i)

instance GameLogic IncGame where

-- A 'GameLogic' instance is tied to a 'Server' instance via the associated type 'ServerT'
  type ServerT IncGame = IncServer

  -- This game doesn't do audio. If we omitted the following associated type declaration,
  -- 'AudioT' would default to 'WithAudio', and if the user doesn't pass the --silent
  -- option, we would pay the price of audio initialization when the game starts.
  --
  -- With the following line, audio is not initialized, and the --silent option is removed
  -- from command line arguments. Note however that if you want to do audio in your game,
  -- and use this tutorial as a template, you'll need to remove this line.
  type AudioT IncGame = ()

  mapInterpretedKey key _ _ = return $ case key of
    -- hitting the space bar will increment the counter, thus changing the size of the frame.
    AlphaNum ' ' -> [CliEvt $ ClientAppEvt IncrementCounter]
    _ -> []

  onClientOnlyEvent = \case
    () -> return ()
  onServerEvent  = \case
    CounterValue value ->
      withAnim $ -- using withAnim will /animate/ the outer frame size change.
        putIGame $ IncGame value

  onClientCustomCmd ResetCounter =
    stateChat $ addMessage $ ChatMessage $ "The counter has been reset."

instance GameDraw IncGame where

  drawForeground (Screen _ screenCenter) _ (IncGame (Counter counterValue)) =
    -- The counter value is drawn in the center of the screen:
    drawStr (show counterValue) screenCenter $ LayeredColor black (rgb 5 4 2)
  -- note that we could have used drawBackground to draw the counter value, it doesn't
  -- make a difference here because in this game we don't use any particle system animation,
  -- hence there is no layer between the foreground layer and the background layer.

instance ServerInit IncServer where

  type ClientViewT IncServer = ()

  mkInitialState _ = return ((), IncServer 0)

  mkInitialClient = ()

instance ServerInParallel IncServer

instance ServerClientLifecycle IncServer where

  clientCanJoin _ = do
    -- A client has just connected, we make it be part of the current game:
    notifyClient' $ EnterState $ Included $ PlayLevel Running
    return True

instance ServerClientHandler IncServer where

  type StateValueT  IncServer = GameStateValue -- This is required

  type ClientEventT IncServer = IncClientEvent

  handleClientEvent IncrementCounter = do
    -- modify the global state (we are inside an MVar transaction so there is
    -- no race condition between different clients):
    modifyState $ \(IncServer n) -> IncServer $ n+1
    -- send the updated value to all clients:
    getsState theServerCounter >>= notifyEveryone . CounterValue
    return []

  acceptCommand ResetCounter = do
    -- modify the global state (we are inside an MVar transaction so there is
    -- no race condition between different clients):
    modifyState $ const $ IncServer 0
    -- send the updated value to all clients:
    getsState theServerCounter >>= notifyEveryone . CounterValue
    return $ Right ()

instance Server IncServer where

  type ServerEventT IncServer = IncServerEvent

  greetNewcomer =
    (:[]) . CounterValue <$> getsState theServerCounter

-- | The player can type commands in the chat window. By default, it is possible to
-- change the player name using @/name newPlayerName@, and to change the color using
-- @/color 3 2 1@. Here, we add a command parser for the @/reset@ command name:
-- with this command, the counter will reset to 0.
instance ServerCmdParser IncServer where

  type CustomCmdT IncServer = IncCommand

  cmdParsers =
    Map.fromList [("reset", parseReset)]
   where
    parseReset = do
      skipSpace
      atEnd >>= bool
        (takeText >>= \t -> error $ unpack $ "Unexpected \"" <> t <> "\" (reset takes no parameter)" )
        (return $ RequestApproval $ CustomCmd ResetCounter)
