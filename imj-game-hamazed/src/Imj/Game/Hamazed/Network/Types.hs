{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
This module exports types related to networking.

Game events are sent by the clients, proccessed by the server. For example, if two players
play the game:

@
  - Ax = acceleration of ship x
  - Lx = laser shot of ship x
  - .  = end of a game period

        >>> time >>>
 . . . A1 . . A1 A2 L2 L1 .
              ^^^^^ ^^^^^
              |     |
              |     laser shots can't be aggregated.
              |
              accelerations can be aggregated, their order within a period is unimportant.
@

The order in which L1 L2 are handled by the server is the order in which they are received.
This is /unfair/ because one player (due to network delays) could have rendered the
last period 100ms before the other, thus having a significant advantage over the other player.
We could be more fair by keeping track of the perceived time on the player side:

in 'ClientAction' we could store the difference between the system time of the action
and the system time at which the last motion update was presented to the player.

Hence, to know how to order close laser shots, if the ships are on the same row or column,
the server should wait a little (max. 50 ms?) to see if the other player makes a
perceptually earlier shot.
-}

module Imj.Game.Hamazed.Network.Types
      ( HamazedServerState
      , HamazedClientEvent(..)
      , HamazedServerEvent(..)
      , HamazedClient
      , RequestsAsyncs(..)
      -- * Player
      , SuggestedPlayerName(..)
      , PlayerName(..)
      , Player(..)
      , PlayerEssence(..)
      , mkPlayer
      , PlayerStatus(..) -- TODO should we merge with 'StateValue' ?
      , PlayerColors(..)
      , mkPlayerColors
      , getPlayerUIName'
      , getPlayerUIName''
      -- * Colors
      , ColorScheme(..)
      -- * Client
      , ClientState(..)
      , StateNature(..)
      , StateValue(..)
      -- * Client / Server communication
      , ClientEvent(..)
      , ServerEvent(..)
      , WorldRequestArg(..)
      , ServerReport(..)
      , Command(..)
      , ClientCommand(..)
      , ServerCommand(..)
      , SharedValueKey(..)
      , SharedEnumerableValueKey(..)
      , SharedValue(..)
      , PlayerNotif(..)
      , GameNotif(..)
      , LeaveReason(..)
      , GameStep(..)
      , GameStatus(..)
      -- * Game
      , GameStateEssence(..)
      , ShotNumber(..)
      , Operation(..)
      , applyOperations
      -- * Utils
      , welcome
      -- * reexport
      , module Imj.Server.Types
      ) where

import           Imj.Prelude

import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.MVar as Lazy -- not using strict version, because Async misses NFData.
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)
import           Data.List(foldl')
import           Data.Set(Set)
import           Data.String(IsString)
import           Data.Text(unpack)

import           Imj.Game.Hamazed.Chat
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Graphics.Font
import           Imj.Graphics.Text.ColorString(ColorString)
import qualified Imj.Graphics.Text.ColorString as ColorString(colored, intercalate)
import           Imj.Graphics.Text.ColoredGlyphList(ColoredGlyphList)
import qualified Imj.Graphics.Text.ColoredGlyphList as ColoredGlyphList(colored)
import           Imj.Server.Types

data ColorScheme =
    UseServerStartTime
  | ColorScheme {-# UNPACK #-} !(Color8 Foreground)
  deriving(Generic, Show)
instance NFData ColorScheme

data ClientState = ClientState {-unpack sum-} !StateNature {-unpack sum-} !StateValue
  deriving(Generic, Show, Eq)

data StateNature = Ongoing | Over
  deriving(Generic, Show, Eq)
instance Binary StateNature

newtype RequestsAsyncs k = RequestsAsyncs (Lazy.MVar (Map k (Set (Async ()))))

data Player = Player {
    getPlayerName :: {-# UNPACK #-} !PlayerName
  , getPlayerStatus :: {-unpack sum-} !PlayerStatus
  , getPlayerColors :: {-# UNPACK #-} !PlayerColors
} deriving(Generic, Show)
instance Binary Player

mkPlayer :: PlayerEssence -> Player
mkPlayer (PlayerEssence a b color) =
  Player a b $ mkPlayerColors color

data PlayerColors = PlayerColors {
    getPlayerColor :: {-# UNPACK #-} !(Color8 Foreground)
    -- ^ color of player name and ship.
  , getColorCycles :: {-# UNPACK #-} !ColorCycles
    -- ^ colors for particle systems
} deriving(Generic, Show, Eq)
instance Binary PlayerColors

mkPlayerColors :: Color8 Foreground -> PlayerColors
mkPlayerColors c = PlayerColors c $ mkColorCycles c

getPlayerUIName' :: Maybe Player -> ColorString
getPlayerUIName' = getPlayerUIName ColorString.colored

getPlayerUIName'' :: Maybe Player -> ColoredGlyphList
getPlayerUIName'' = getPlayerUIName (ColoredGlyphList.colored . map textGlyph . unpack)

getPlayerUIName :: (IsString a, Semigroup a)
                => (Text -> Color8 Foreground -> a)
                -> Maybe Player
                -> a
-- 'Nothing' happens when 2 players disconnect while playing: the first one to reconnect will not
-- know about the name of the other disconnected player, until the other player reconnects (TODO is it still the case?).
getPlayerUIName _ Nothing = "? (away)"
getPlayerUIName f (Just (Player (PlayerName name) status (PlayerColors c _))) =
  case status of
    Present -> n
    Absent  -> n <> f " (away)" chatMsgColor
 where
  n = f name c

data Command =
    ClientCmd !ClientCommand
  | ServerCmd !ServerCommand
  | ServerRep !ServerReport
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary Command

applyOperations :: [ShotNumber] -> Int
applyOperations =
  foldl' (\v (ShotNumber n op) ->
            case op of
              Add -> v + n
              Substract -> v - n) 0

welcome :: Map ShipId Player -> ColorString
welcome l =
  text "Welcome! Players are: "
  <> ColorString.intercalate
      (text ", ")
      (map (getPlayerUIName' . Just) $ Map.elems l)
 where
  text x = ColorString.colored x chatMsgColor
