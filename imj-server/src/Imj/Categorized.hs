{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Categorized
        ( Categorized(..)
        , EventCategory(..)
        , evtCatToRep
        , evtRepToCS
        ) where

import           Imj.Prelude

import           Data.Text(singleton)

import           Imj.Graphics.Color
import           Imj.Graphics.Text.ColorString

class (Show e) => Categorized e where
  -- | 'evtCategory' is used for debugging purposes, to provide a single-character
  -- representations of events.
  --
  -- The default implementation assigns all event values to the same category.
  evtCategory :: e -> EventCategory
  evtCategory _ = Command'

instance Categorized ()

instance (Categorized a, Categorized b)
  => Categorized (Either a b) where
  evtCategory = either evtCategory evtCategory

data EventCategory =
  -- general
    ToggleEventRecording'
  | ConnectionAccepted'
  | ConnectionRefused'
  | Disconnected'
  | Command'
  | Error'
  -- when an overdue deadline was ignored because its priority was lower than another overdue deadline
  | IgnoredOverdue
  -- visual
  | CycleRenderingOptions'
  -- game config
  | WorldRequest'
  | ChangeLevel'
  | EndLevel'
  | EnterState'
  | ExitState'
  -- game play
  | PeriodicMotion'
  | MoveFlyingItems'
  | Laser'
  -- game render
  | AnimateParticleSystem'
  | AnimateUI'
  -- chat
  | Chat'
  deriving(Eq, Show)

data EventRepr = EventRepr {-# UNPACK #-} !Char {-# UNPACK #-} !(Color8 Foreground)

evtRepToCS :: EventRepr -> ColorString
evtRepToCS (EventRepr c color) = colored (singleton c) color

evtCatToRep :: EventCategory -> EventRepr
evtCatToRep IgnoredOverdue = EventRepr 'X' red
evtCatToRep ChangeLevel'  = EventRepr 'C' magenta
evtCatToRep WorldRequest' = EventRepr 'R' magenta
evtCatToRep AnimateUI'    = EventRepr 'U' magenta
evtCatToRep ConnectionAccepted'    = EventRepr 'A' cyan
evtCatToRep Chat'                  = EventRepr 'C' cyan
evtCatToRep Disconnected'          = EventRepr 'D' cyan
evtCatToRep EndLevel'              = EventRepr 'E' cyan
evtCatToRep EnterState'            = EventRepr 'I' cyan
evtCatToRep Laser'                 = EventRepr 'L' cyan
evtCatToRep ExitState'             = EventRepr 'O' cyan
evtCatToRep ConnectionRefused'     = EventRepr 'R' cyan
evtCatToRep Error'                 = EventRepr 'X' cyan
evtCatToRep Command'         = EventRepr 'C' yellow
evtCatToRep CycleRenderingOptions' = EventRepr 'R' yellow
evtCatToRep ToggleEventRecording'  = EventRepr 'T' yellow
evtCatToRep MoveFlyingItems' = EventRepr 'M' green
evtCatToRep AnimateParticleSystem' = EventRepr 'P' blue
evtCatToRep PeriodicMotion'        = EventRepr 'S' blue
