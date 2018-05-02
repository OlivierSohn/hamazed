{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Create
        ( mkGame
        , mkWorld
        , worldToEssence
        ) where

import           Imj.Prelude

import qualified Data.Map as Map

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Graphics.Screen
import           Imj.ServerView.Types

import           Imj.Game.Hamazed.World.Space.Draw

mkGame :: Screen
       -> ConnectIdT (ServerT g)
       -> ServerView (ServerT g)
       -> ConnectionStatus
       -> g
       -> Game g
mkGame screen suggPlayerName server connectionStatus g =
  Game (ClientState Ongoing Excluded) screen g [] mempty suggPlayerName server connectionStatus mkChat

mkWorld :: WorldEssence -> Maybe WorldId -> World
mkWorld (WorldEssence balls ships space) wid =
  let renderedSpace = mkRenderedSpace space
  in World (Map.map mkNumber balls) ships space renderedSpace mempty wid

worldToEssence :: World -> (WorldEssence, Maybe WorldId)
worldToEssence (World balls ships space _ _ wid) =
  (WorldEssence (Map.map getNumEssence balls) ships space, wid)
