{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.DeepSeq(NFData)
import           Control.Monad.State.Strict(modify', gets)
import           Data.Proxy(Proxy(..))
import           Data.Binary(Binary(..))
import           GHC.Generics(Generic)

import           Imj.Categorized
import           Imj.Game.App(runGame)
import           Imj.Game.Command
import           Imj.Game.Infos
import           Imj.Game.Status
import           Imj.Game.Types
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Screen
import           Imj.Graphics.Render.FromMonadReader(drawStr)
import           Imj.Graphics.UI.RectContainer
import           Imj.Input.Types
import           Imj.Network
import           Imj.Server
import           Imj.Server.Class
import           Imj.Server.Connection
import           Imj.Server.Types
import           Imj.Util

main :: IO ()
main =
  -- we use 'Proxy' to "tell" the type system what is the type of game.
  runGame (Proxy :: Proxy IncGame)

type Counter = Int

-- | This implements the client-side logic of the game
data IncGame = IncGame {
  theClientCounter :: !Counter
}

evenTriangle :: Int -> Int
evenTriangle = (*) 2 . zigzag 0 5

instance GameLogic IncGame where
  type ServerT        IncGame = IncServer
  type ClientOnlyEvtT IncGame = ()
  type ColorThemeT    IncGame = ()
  type ClientInfoT    IncGame = ()

  getFrameColor _ = onBlack $ rgb 2 1 1

  getViewport _ (Screen _ center) (IncGame n) =
    let i = evenTriangle n
    in mkRectContainerWithCenterAndInnerSize center $ Size (10+fromIntegral i) (10+fromIntegral i)

  keyMaps key _ = return $ case key of
    AlphaNum ' ' -> Just $ CliEvt $ ClientAppEvt IncrementCounter
    _ -> Nothing

  onCustomEvent (Right ()) =
    return ()
  onCustomEvent (Left (CounterValue value)) =
    -- The frame size depends on the counter (cf 'getViewport') so we use 'withAnim'
    -- to allow the frame to animate.
    withAnim Normal (return ()) $
      putIGame $ IncGame value

  drawGame =
    gets game >>= \(Game _ (Screen _ screenCenter) (GameState mayG _) _ _ _ _ _ _ _) ->
      maybe (return ())
        (\(IncGame counterValue) -> drawStr (show counterValue) screenCenter $ LayeredColor black (rgb 5 4 2))
        mayG

-- | This implements the server-side logic of the game
data IncServer = IncServer {
  theServerCounter :: !Counter
} deriving(NFData, Generic)

instance Server IncServer where
  type StateValueT      IncServer = GameStateValue

  type ConnectIdT                IncServer = ClientName Proposed
  type ReconnectionContext       IncServer = ()
  type ClientEventT              IncServer = IncClientEvent
  type ServerEventT              IncServer = IncServerEvent

  type ValuesT            IncServer = ()
  type ClientViewT               IncServer = ()

  type ValueKeyT           IncServer = ()
  type ValueT              IncServer = ()
  type EnumValueKeyT IncServer = ()

  mkInitial _ =
    return ((),IncServer 0)

  acceptConnection _ =
    Right ()

  mkInitialClient = ()

  greetNewcomer =
    (:[]) . CounterValue . theServerCounter <$> gets unServerState

  getValue () = return ()
  onPut ()    = return ()
  onDelta _ _ = return ()

  clientCanJoin _ = do
    -- A client has just connected, we make it be part of the current game:
    notifyClient' $ EnterState $ Included $ PlayLevel Running
    return True

  handleClientEvent IncrementCounter = do
    -- modify the global state (we are inside an MVar transaction so there is
    -- no race condition between different clients):
    modify' $ mapState $ \(IncServer n) -> IncServer $ n+1
    -- send the updated value to all clients:
    gets' theServerCounter >>= notifyEveryone . CounterValue

-- | This represents events generated by the client and sent to the server.
data IncClientEvent =
    IncrementCounter
  deriving(Show,Binary,Categorized,Generic)

-- | This represents events generated by the server, sent to the clients.
data IncServerEvent =
    CounterValue !Counter
  deriving(Show,Binary,Categorized,Generic)
