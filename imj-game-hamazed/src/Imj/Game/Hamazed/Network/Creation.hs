{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Network.Creation
      ( addStats
      , participateToWorldCreation
      ) where

import           Imj.Prelude

import           Control.Monad.Reader(asks)
import           Control.Monad.State.Strict(modify', gets)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text(pack)

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Color.Types
import           Imj.Server.Internal.Types

import           Imj.Graphics.Text.ColorString(colored)
import           Imj.Server.Log

addStats :: Map Properties Statistics
         -> WorldId
         -- ^ if this 'WorldId' doesn't match with the 'WorldId' of 'worldCreation',
         -- nothing is done because it is obsolete (eventhough the server cancels obsolete requests,
         -- this case could occur if the request was cancelled just after the non-result was
         -- sent by the client).
         -> ClientHandlerIO HamazedServerState ()
addStats stats key =
  gets' worldCreation >>= \wc@(WorldCreation st wid _ prevStats) -> do
    let newStats = safeMerge mergeStats prevStats stats
    bool
      (serverLog $ pure $ colored ("Obsolete key " <> pack (show key)) blue)
      (case st of
          -- drop newStats if world is already created.
        Created -> return ()
        CreationAssigned _ -> do
          log $ colored (pack $ show newStats) white
          modify' $ mapState $ \s -> s { worldCreation = wc { creationStatistics = newStats } })
      $ wid == key


participateToWorldCreation :: WorldId
                           -- ^ if this 'WorldId' doesn't match with the 'WorldId' of 'worldCreation',
                           -- nothing is done because it is obsolete (eventhough the server cancels obsolete requests,
                           -- this case could occur if the request was cancelled just after the non-result was
                           -- sent by the client).
                           -> ClientHandlerIO HamazedServerState ()
participateToWorldCreation key = asks clientId >>= \origin ->
  gets' worldCreation >>= \wc@(WorldCreation st wid _ _) ->
    bool
      (serverLog $ pure $ colored ("Obsolete key " <> pack (show key)) blue)
      (case st of
        Created ->
          serverLog $ pure $ colored ("World " <> pack (show key) <> " is already created.") blue
        CreationAssigned assignees -> do
          let prevSize = Set.size assignees
              single = Set.singleton origin
              newAssignees = Set.union single assignees
              newSize = Set.size newAssignees
          unless (newSize == prevSize) $
            -- a previously assigned client has disconnected, reconnects and sends a non-result
            serverLog $ pure $ colored ("Adding assignee : " <> pack (show origin)) blue
          modify' $ mapState $ \s -> s { worldCreation = wc { creationState = CreationAssigned newAssignees
                                                 } }
          gets clientsMap >>= requestWorldBy . flip Map.restrictKeys single)
      $ wid == key
