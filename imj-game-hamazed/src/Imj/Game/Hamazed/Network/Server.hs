{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Network.Server
      ( mkServer
      , defaultPort
      ) where

import           Imj.Prelude

import           Data.Char (toLower)

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Graphics.Color.Types


defaultPort :: ServerPort
defaultPort = ServerPort 10052


mkServer :: Maybe ColorScheme -> Maybe ServerLogs -> Maybe ServerName -> ServerContent WorldParameters -> HamazedView
mkServer color logs Nothing =
  ServerView (Local (fromMaybe NoLogs logs) (fromMaybe (ColorScheme $ rgb 3 2 2) color))
mkServer Nothing Nothing (Just (ServerName n)) =
  ServerView (Distant $ ServerName $ map toLower n)
mkServer _ (Just _) (Just _) =
  error "'--serverLogs' conflicts with '--serverName' (these options are mutually exclusive)."
mkServer (Just _) _ (Just _) =
  error "'--colorScheme' conflicts with '--serverName' (these options are mutually exclusive)."
