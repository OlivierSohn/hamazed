{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Graphics.UI.Chat
  ( Chat(..)
  , mkChat
  , takeMessage
  , chatMsgColor
  , chatWinColor
  , IsEditing(..)
  , EditableText(..)
  , ChatMessage(..)
  , addMessage
  , runChat
  , ChatCommand(..)
  ) where

import           Imj.Prelude hiding (drop, null)
import qualified Prelude as P(length)
import           Data.Text(Text, snoc, length, splitAt, dropEnd, drop, null)

import           Imj.Graphics.Class.Positionable
import qualified Imj.Graphics.Class.Words as Words
import           Imj.Graphics.Color
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.UI.TextBox
import           Imj.Geo.Discrete
import           Imj.Log
import           Imj.Network

data Chat = Chat {
    editableText :: !EditableText
  -- ^ The message that is being typed
  , getIsEditing :: !IsEditing
  -- ^ Whether chat is in edit mode or not.
  , _otherPlayersTyping :: ![ClientName]
  -- ^ Players currently having a pending (unsent) message.
  , renderedChat :: !(TextBox ColorString)
}
instance Positionable Chat where
  drawAt (Chat pending edit _ box) pos = do
    drawAt box pos
    let txtPos = translate (Coords (pred $ fromIntegral $ height box) 0) pos
        doc' = case edit of
          NotEditing -> [ "[Press 'Tab' to chat]"
                        ]
          Editing -> [ "[Press 'Enter' to send]"
                     , "[Use '/' to issue a command]"
                     , "[Press 'Tab' to quit]"
                     ]
        doc = map (`colored'` pendingTextColors) doc'
        str = editableTxtToColorStr edit pending
        w = 30
        -- this line replaced by lines below because the multiline algorithm fails, when spaces
        -- contain needed color information, cf. Backlog.
        --editBox = addText str $ mkAdjustableHeightTextBox w
        multi = reverse . Words.multiLineTrivial w
        allLines = concatMap multi $ reverse $ str : doc
        editBox = TextBox (Size (fromIntegral $ P.length allLines) w) [allLines] Adjust NoColor
    drawAt editBox txtPos
    -- TODO draw others
  width (Chat _ _ _ box) = width box
  height (Chat _ _ _ box) = height box + 2 -- for pending text
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}
  {-# INLINABLE drawAt #-}

data EditableText = EditableText {
    getText :: !Text
  , editingPos :: !Int
}

data IsEditing =
    Editing
  | NotEditing

toggleIsEditing :: IsEditing -> IsEditing
toggleIsEditing Editing = NotEditing
toggleIsEditing NotEditing = Editing

editableTxtToColorStr :: IsEditing -> EditableText -> ColorString
editableTxtToColorStr Editing (EditableText txt editPos) =
  colored' start pendingTextColors <>
  colored' edit pendingTextColorsEdited <>
  colored' end pendingTextColors
 where
  (start, t2) = splitAt editPos txt
  (edit, end)
    | null t2 = (" ", mempty)
    | otherwise = splitAt 1 t2
editableTxtToColorStr NotEditing (EditableText txt _) =
  colored' txt pendingTextColorsInactive

data ChatCommand =
    ToggleEditing
   -- ^ Enter or exit chat "text edit" mode
  | Insert !Char
  | DeleteAtEditingPosition
  | DeleteBeforeEditingPosition
  | Navigate {-unpack sum-} !Direction
  -- ^ Up / Down to recall past messages, LEFT / RIGHT to change edit position
  deriving(Eq, Show)

mkChat :: Chat
mkChat = Chat mkEditableText NotEditing [] (mkTextBox (Size 10 30) $ Alternate False)

mkEditableText :: EditableText
mkEditableText = EditableText mempty 0


data ChatMessage =
    ChatMessage !ColorString
  | Information !MessageLevel !Text

chatMsgColor :: Color8 Foreground
chatMsgColor = gray 10

chatWinColor :: Color8 Foreground
chatWinColor = gray 14

toColorStr :: ChatMessage -> ColorString
toColorStr (ChatMessage c) = c
toColorStr (Information Info s) = colored s $ chartreuse `mix` chatMsgColor
toColorStr (Information Warning s) = colored s yellow
toColorStr (Information Error s) = colored s red

addMessage :: ChatMessage -> Chat -> (Chat, ())
addMessage msg c =
  (c { renderedChat = addText (toColorStr msg) $ renderedChat c }, ())

takeMessage :: Chat -> (Chat, Text)
takeMessage c@(Chat (EditableText txt _) _ _ _) =
  (c { editableText = mkEditableText }, txt)

runChat :: ChatCommand -> Chat -> Chat
runChat ToggleEditing c@(Chat _ edit _ _) =
  c { getIsEditing = toggleIsEditing edit }
runChat (Insert char) c@(Chat (EditableText txt pos) _ _ _) =
  let (t1, t2) = splitAt pos txt
  in c { editableText = EditableText (snoc t1 char <> t2) $ succ pos }
runChat DeleteAtEditingPosition c@(Chat (EditableText txt pos) _ _ _) =
  let (t1, t2) = splitAt pos txt
      newT2 = drop 1 t2
  in c { editableText = EditableText (t1 <> newT2) $ length t1 }
runChat DeleteBeforeEditingPosition c@(Chat (EditableText txt pos) _ _ _) =
  let (t1, t2) = splitAt pos txt
      newT1 = dropEnd 1 t1
  in c { editableText = EditableText (newT1 <> t2) $ length newT1 }
runChat (Navigate dir) c@(Chat e@(EditableText txt pos) _ _ _) =
  let newPos = case dir of
        LEFT -> max 0 $ pred pos
        RIGHT -> min (length txt) $ succ pos
        Up -> 0
        Down -> length txt
  in c { editableText = e { editingPos = newPos } }

pendingTextColors, pendingTextColorsInactive, pendingTextColorsEdited :: LayeredColor
pendingTextColors = onBlack $ gray 12
pendingTextColorsInactive = pendingTextColors
pendingTextColorsEdited = LayeredColor (gray 5) $ gray 12
