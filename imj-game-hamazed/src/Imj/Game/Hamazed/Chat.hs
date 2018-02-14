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
import           Data.Text(Text)

data Chat = Chat {
    chatMessages :: ![ChatMessage]
  , _pendingMessage :: !Text
  -- ^ The message that is being typed
  , _otherPlayersTyping :: ![PlayerName]
  -- ^ Players currently having a pending (unsent) message.
}

mkChat :: Chat
mkChat = Chat [] mempty []

newtype PlayerName = PlayerName Text
  deriving(Generic, Show, Binary, Eq, NFData)

data ChatMessage =
    ChatMessage !Text
  | DisconnectionReason !String

addMessage :: ChatMessage -> Chat -> Chat
addMessage msg c = c { chatMessages = msg:(chatMessages c) }
