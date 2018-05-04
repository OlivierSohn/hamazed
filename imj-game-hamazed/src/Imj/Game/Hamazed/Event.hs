{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Event
        ( ActionTarget(..)
        , MetaAction(..)
        , HamazedEvent(..)
        ) where

import           Imj.Prelude

import           Imj.Event

data HamazedEvent =
     Interrupt !MetaAction
   -- ^ A game interruption.
   | PlayProgram !Int
   deriving(Eq, Show)
instance Categorized HamazedEvent where
  evtCategory = \case
    PlayProgram{}   -> Command'
    Interrupt _ -> Interrupt'

data MetaAction = Help
                -- ^ The player wants to read the help page /(Not implemented yet)/
                deriving(Eq, Show)

data ActionTarget = Ship
                  -- ^ The player wants to accelerate the 'BattleShip'
                  | Laser
                  -- ^ The player wants to shoot with the laser.
                  deriving(Generic, Eq, Show, Binary)
