{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Game.Show
      ( getPlayerUIName'
      , getPlayerUIName''
      , welcome
      ) where

import           Imj.Prelude

import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)
import           Data.String(IsString)
import           Data.Text(unpack)

import           Imj.ClientView.Types
import           Imj.Graphics.UI.Chat
import           Imj.Game.Status
import           Imj.Game.Types
import           Imj.Graphics.Color
import           Imj.Graphics.Font
import           Imj.Graphics.Text.ColorString(ColorString)
import qualified Imj.Graphics.Text.ColorString as ColorString(colored, intercalate)
import           Imj.Graphics.Text.ColoredGlyphList(ColoredGlyphList)
import qualified Imj.Graphics.Text.ColoredGlyphList as ColoredGlyphList(colored)
import           Imj.Network

getPlayerUIName' :: Maybe (Player g) -> ColorString
getPlayerUIName' = getPlayerUIName (ColorString.colored . unClientName)

getPlayerUIName'' :: Maybe (Player g) -> ColoredGlyphList
getPlayerUIName'' = getPlayerUIName (ColoredGlyphList.colored . map textGlyph . unpack . unClientName)

getPlayerUIName :: (IsString a, Semigroup a)
                => (ClientName Approved -> Color8 Foreground -> a)
                -> Maybe (Player g)
                -> a
-- 'Nothing' happens when 2 players disconnect while playing: the first one to reconnect will not
-- know about the name of the other disconnected player, until the other player reconnects (TODO is it still the case?).
getPlayerUIName _ Nothing = "? (away)"
getPlayerUIName f (Just (Player name status (PlayerColors c _))) =
  case status of
    Present -> n
    Absent  -> n <> f " (away)" chatMsgColor
 where
  n = f name c

welcome :: Map ClientId (Player g) -> ColorString
welcome l =
  text "Welcome! Players are: "
  <> ColorString.intercalate
      (text ", ")
      (map (getPlayerUIName' . Just) $ Map.elems l)
 where
  text x = ColorString.colored x chatMsgColor
