{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Create
        ( mkInitialState
        , mkIntermediateState
        , initialGameState
        , initialGame
        , mkGameStateEssence
        ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Map as Map

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Screen
import           Imj.ServerView.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Space.Draw
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer

initialGame :: Screen
            -> SuggestedPlayerName
            -> HamazedView
            -> ConnectionStatus
            -> IO Game
initialGame screen suggPlayerName server connectionStatus =
  initialGameState initialParameters initialViewMode screen >>= \s ->
    return $ Game (ClientState Ongoing Excluded) screen s [] mempty suggPlayerName server connectionStatus mkChat

initialGameState :: WorldParameters
                 -> ViewMode
                 -> Screen
                 -> IO GameState
initialGameState _ mode s =
  mkInitialState mkEmptyLevelEssence mkMinimalWorldEssence Nothing mempty mode s Nothing

mkInitialState :: (MonadIO m)
               => LevelEssence
               -> WorldEssence
               -> Maybe WorldId
               -> Map ShipId Player
               -> ViewMode
               -> Screen
               -> Maybe GameState
               -> m GameState
mkInitialState = mkIntermediateState []

mkIntermediateState :: (MonadIO m)
                    => [ShotNumber]
                    -> LevelEssence
                    -> WorldEssence
                    -> Maybe WorldId
                    -> Map ShipId Player
                    -> ViewMode
                    -> Screen
                    -> Maybe GameState
                    -> m GameState
mkIntermediateState newShotNums newLevel essence wid names mode (Screen _ screenCenter) mayState = do
  let newWorld@(World _ _ space _ _ _) = mkWorld essence wid
      (curWorld@(World _ _ curSpace _ _ _), level, shotNums) =
        maybe
        (newWorld, newLevel, [])
        (\(GameState w _ curShotNums (Level curLevel _) _ _) ->
            (w, curLevel, curShotNums))
          mayState
      curInfos = mkInfos Normal        (getWorldShips curWorld) names shotNums    level
      newInfos = mkInfos ColorAnimated (getWorldShips newWorld) names newShotNums newLevel
      (horizontalDist, verticalDist) = computeViewDistances mode
  kt <- liftIO getSystemTime
  let uiAnimation =
        mkUIAnimation
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize screenCenter (getSize curSpace), curInfos)
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize screenCenter (getSize space), newInfos)
          horizontalDist verticalDist kt
          -- only when UIAnimation is over, curWorld will be replaced by newWorld.
          -- during UIAnimation, we need the two worlds.
  return $ GameState curWorld (Just newWorld) newShotNums (mkLevel newLevel) uiAnimation mode

mkGameStateEssence :: WorldId -> GameState -> Maybe GameStateEssence
mkGameStateEssence wid' (GameState curWorld mayNewWorld shotNums (Level levelEssence _) _ _) =
  let (essence, mayWid) = worldToEssence $ fromMaybe curWorld mayNewWorld
  in maybe
    Nothing
    (\wid -> bool Nothing (Just $ GameStateEssence essence shotNums levelEssence) $ wid == wid')
    mayWid

mkWorld :: WorldEssence -> Maybe WorldId -> World
mkWorld (WorldEssence balls ships space) wid =
  let renderedSpace = mkRenderedSpace space
  in World (Map.map mkNumber balls) ships space renderedSpace mempty wid

worldToEssence :: World -> (WorldEssence, Maybe WorldId)
worldToEssence (World balls ships space _ _ wid) =
  (WorldEssence (Map.map getNumEssence balls) ships space, wid)
