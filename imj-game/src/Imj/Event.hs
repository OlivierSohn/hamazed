{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Event
        ( Event(..)
        , Deadline(..)
        , DeadlineType(..)
        , EventCategory(..)
        , ParticleSystemKey(..)
        -- * Reexports (for haddock hyperlinks)
        , module Imj.Graphics.ParticleSystem.Design.Create
        ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types
import           Imj.Categorized

import           Imj.Graphics.UI.Chat
import           Imj.Graphics.ParticleSystem.Design.Create
import           Imj.Graphics.Font
import           Imj.Iteration
import           Imj.Log
import           Imj.Timing

-- | A foreseen game or animation update.
data Deadline = Deadline {
    _deadlineTime :: {-# UNPACK #-} !(Time Point System)
    -- ^ At which time should the update become visible to the user.
  , _deadlinePriority :: {-# UNPACK #-} !Int
  , _deadlineType :: {-unpack sum-} !DeadlineType
} deriving(Eq, Show)


data DeadlineType = AnimateParticleSystem {-# UNPACK #-} !ParticleSystemKey
                  -- ^ Update one or more 'ParticleSystem's.
                  | AnimateUI
                  -- ^ Update the inter-level animation
                  | RedrawStatus {-# UNPACK #-} !Frame {-# UNPACK #-} !Int
                  -- ^ Draw the status at that 'Frame', using that line index.
                  | PollExternalEvents
                  deriving(Eq, Show)

data Event e =
    Timeout {-# UNPACK #-} !Deadline
  -- ^ The 'Deadline' needs to be handled immediately.
  | Log !MessageLevel !Text
  | ToggleEventRecording
  | CanvasSizeChanged
  -- ^ Produced by the delta renderer when a size change was detected upon rendering.
  | RenderingTargetChanged
  | CycleRenderingOptions {-# UNPACK #-} !CycleFont {-# UNPACK #-} !CycleFontSize
  -- ^ Changes the font used to render
  | ApplyPPUDelta {-# UNPACK #-} !Size
  | ApplyFontMarginDelta {-# UNPACK #-} !FontMargin
  -- ^ Produced by the platform
  | ChatCmd {-unpack sum-} !ChatCommand
  | SendChatMessage
  -- ^ Send message or execute command if the message starts with a '/'
  | AppEvent !e
  | SequenceOfEvents [Event e]
  deriving(Generic, Show, Eq)
instance (Categorized e) => Categorized (Event e) where
  evtCategory = \case
    SequenceOfEvents {} -> Command'
    Log _ _         -> Command'
    SendChatMessage -> Command'
    ChatCmd _       -> Command'
    Timeout (Deadline _ _ (AnimateParticleSystem _)) -> AnimateParticleSystem'
    Timeout (Deadline _ _ AnimateUI)    -> AnimateUI'
    Timeout (Deadline _ _ RedrawStatus {}) -> AnimateUI'
    Timeout (Deadline _ _ PollExternalEvents) -> AnimateUI'
    ToggleEventRecording -> ToggleEventRecording'
    CanvasSizeChanged         -> CycleRenderingOptions'
    RenderingTargetChanged    -> CycleRenderingOptions'
    ApplyPPUDelta _           -> CycleRenderingOptions'
    ApplyFontMarginDelta _    -> CycleRenderingOptions'
    CycleRenderingOptions _ _ -> CycleRenderingOptions'
    AppEvent e -> evtCategory e

newtype ParticleSystemKey = ParticleSystemKey Int
  deriving (Eq, Ord, Enum, Show, Num)
