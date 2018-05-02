{-# OPTIONS_HADDOCK hide #-}
-- | Contains types that the game server doesn't need to know about.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Game.Hamazed.Types
    ( GracefulProgramEnd(..)
    , UnexpectedProgramEnd(..)
    , Game(..)
    , GameTime
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

-- import           Imj.Client.Class
import           Imj.Graphics.RecordDraw
import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.World.Types
import           Imj.Game.Hamazed.State.Types

import           Imj.Game.Hamazed.Chat
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.UI.Animation
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
