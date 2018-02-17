{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Game.Hamazed.Chat
  ( Chat(..)
  , mkChat
  , ChatMessage(..)
  , addMessage
  , PlayerName(..)
  ) where

import           Imj.Prelude
import           Control.DeepSeq(NFData)
import           Data.Text(Text, pack)

import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Color
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.UI.TextBox
import           Imj.Geo.Discrete

data Chat = Chat {
    _pendingMessage :: !Text
  -- ^ The message that is being typed
  , _otherPlayersTyping :: ![PlayerName]
  -- ^ Players currently having a pending (unsent) message.
  , _renderedChat :: !(TextBox ColorString)
}
instance Positionable Chat where
  drawAt (Chat _ _ box) = drawAt box
    -- TODO draw pending, and others
  width (Chat _ _ box) = width box
  {-# INLINABLE width #-}
  {-# INLINABLE drawAt #-}

mkChat :: Chat
mkChat = Chat mempty [] (mkTextBox $ Size 10 30)

newtype PlayerName = PlayerName Text
  deriving(Generic, Show, Binary, Eq, NFData)

data ChatMessage =
    ChatMessage !Text
  | DisconnectionReason !String

toColorStr :: ChatMessage -> ColorString
toColorStr (ChatMessage s) = colored s yellow
toColorStr (DisconnectionReason s) = colored (pack s) red

addMessage :: ChatMessage -> Chat -> Chat
addMessage msg (Chat p o box) =
  Chat p o $ addText (toColorStr msg) box
