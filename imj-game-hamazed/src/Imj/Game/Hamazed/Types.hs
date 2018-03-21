{-# OPTIONS_HADDOCK hide #-}
-- | Contains types that the game server doesn't need to know about.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Hamazed.Types
    ( GracefulProgramEnd(..)
    , UnexpectedProgramEnd(..)
    , Game(..)
    , GameTime
    , GameState(..)
    , AnimatedLine(..)
    , UpdateEvent
    , EventGroup(..)
    , GenEvent(..)
    , initialParameters
    , initialViewMode
    , minRandomBlockSize
    -- * Reexports
    , module Imj.Game.Hamazed.Chat
    , module Imj.Game.Hamazed.Level.Types
    , module Imj.Game.Hamazed.World.Types
    , UIAnimation
    , RecordDraw
    ) where

import           Imj.Prelude
import           Control.Exception.Base(Exception(..))
import           Data.Map.Strict(Map)
import           Data.Text(unpack)

import           Imj.Graphics.RecordDraw
import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Types
import           Imj.Game.Hamazed.World.Space.Types

import           Imj.Game.Hamazed.Chat
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.Text.ColorString


-- Note that we don't have GracefulClientEnd, because we use exitSuccess in that case
-- and don't need an exception.
data GracefulProgramEnd =
    GracefulServerEnd
instance Exception GracefulProgramEnd
instance Show GracefulProgramEnd where
  show GracefulServerEnd        = withNewline "Graceful server shutdown."

data UnexpectedProgramEnd =
    UnexpectedProgramEnd !Text
  | ErrorFromServer !String
instance Exception UnexpectedProgramEnd
instance Show UnexpectedProgramEnd where
  show (UnexpectedProgramEnd s) = withNewline $ unpack s
  show (ErrorFromServer s)      = withNewline $ "An error occured in the Server: " ++ s

withNewline :: String -> String
withNewline = flip (++) "\n"


data EventGroup = EventGroup {
    events :: ![UpdateEvent]
  , _eventGroupHasPrincipal :: !Bool
  , _eventGroupUpdateDuration :: !(Time Duration System)
  , _eventGroupVisibleTimeRange :: !(Maybe (TimeRange System))
  -- ^ TimeRange of /visible/ events deadlines
}

-- | Regroups events that can be handled immediately by the client.
type UpdateEvent = Either ServerEvent Event

{- Regroups all kinds of events. -}
data GenEvent =
    Evt {-unpack sum-} !Event
    -- ^ Are generated by the client and can be handled by the client immediately.
  | CliEvt {-unpack sum-} !ClientEvent
    -- ^ Are generated by the client but can't be handled by the client, and are sent to the game server.
  | SrvEvt {-unpack sum-} !ServerEvent
    -- ^ Are generated by the game server or by the client, and can be handled by the client immediately.
    deriving(Generic, Show)

data Game = Game {
    getClientState :: {-# UNPACK #-} !ClientState
  , getGameState' :: !GameState
  , _gameSuggestedPlayerName :: {-unpack sum-} !SuggestedPlayerName
  , getServer :: {-unpack sum-} !Server
  -- ^ The server that runs the game
  , connection' :: {-unpack sum-} !ConnectionStatus
  , getChat' :: !Chat
}

{-| 'GameState' has two fields of type 'World' : during 'Level' transitions,
we draw the /old/ 'World' while using the /new/ 'World' 's
dimensions to animate the UI accordingly. -}
data GameState = GameState {
    currentWorld :: !World
  , mayFutureWorld :: !(Maybe World)
    -- ^ Maybe the world that we transition to (when a level is over).
    -- Once the transition is over, we replace 'currentWorld' with this 'Just' value.
  , _gameStateShotNumbers :: ![ShotNumber]
    -- ^ Which 'Number's were shot
  , getGameLevel :: !Level
    -- ^ The current 'Level'
  , getUIAnimation :: !UIAnimation
    -- ^ Inter-level animation.
  , getDrawnClientState :: [( ColorString -- The raw message, just used to compare with new messages. For rendering,
                                          -- AnimatedLine is used.
                          , AnimatedLine)]
  , getScreen :: {-# UNPACK #-} !Screen
  , getViewMode' :: {-unpack sum-} !ViewMode
  , getPlayers' :: !(Map ShipId Player)
}

data AnimatedLine = AnimatedLine {
    getRecordDrawEvolution :: !(Evolution RecordDraw)
  , getALFrame :: !Frame
  , getALDeadline :: Maybe Deadline
} deriving(Generic, Show)

minRandomBlockSize :: Int
minRandomBlockSize = 6 -- using 4 it once took a very long time (one minute, then I killed the process)
                       -- 6 has always been ok

initialParameters :: WorldParameters
initialParameters = WorldParameters Rectangle2x1 (Random defaultRandom)

initialViewMode :: ViewMode
initialViewMode = CenterSpace

defaultRandom :: RandomParameters -- below 0.1, it's difficult to have 2 or more connected components.
                                  -- 0.1 : on avg, 1 cc
                                  -- 0.2 : on avg, 1 cc
                                  -- 0.3 : on avg, 2 cc
                                  -- 0.4 : on avg, 5 cc
                                  -- 0.5 : on avg, 8 cc
                                  -- 0.6 : on avg, 10 cc
                                  -- above 0.6, it's difficult to have a single connected component
defaultRandom = RandomParameters minRandomBlockSize 0.5
