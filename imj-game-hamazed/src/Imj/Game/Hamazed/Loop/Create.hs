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
import           Imj.ServerView.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Space.Draw
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer

initialGame :: Maybe Size
            -> SuggestedPlayerName
            -> HamazedView
            -> ConnectionStatus
            -> IO Game
initialGame ms suggPlayerName server connectionStatus =
  initialGameState initialParameters initialViewMode ms >>= \s ->
    return $ Game (ClientState Ongoing Excluded) s suggPlayerName server connectionStatus mkChat

initialGameState :: WorldParameters
                 -> ViewMode
                 -> Maybe Size
                 -> IO GameState
initialGameState _ mode ms =
  mkInitialState mkEmptyLevelEssence mkMinimalWorldEssence Nothing mempty mode ms Nothing

mkInitialState :: (MonadIO m)
               => LevelEssence
               -> WorldEssence
               -> Maybe WorldId
               -> Map ShipId Player
               -> ViewMode
               -> Maybe Size
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
                    -> Maybe Size
                    -> Maybe GameState
                    -> m GameState
mkIntermediateState newShotNums newLevel essence wid names mode maySz mayState = do
  let screen@(Screen _ newScreenCenter) = mkScreen maySz
      newWorld@(World _ _ space _ _ _) = mkWorld essence wid
      (curWorld@(World _ _ curSpace _ _ _), curScreenCenter, level, shotNums, stAnim) =
        maybe
        (newWorld, newScreenCenter, newLevel, [], [])
        (\(GameState w _ curShotNums (Level curLevel _) _ stateAnim (Screen _ center) _ _) ->
            (w, center, curLevel, curShotNums, stateAnim))
          mayState
      curInfos = mkInfos Normal        (getWorldShips curWorld) names shotNums    level
      newInfos = mkInfos ColorAnimated (getWorldShips newWorld) names newShotNums newLevel
      (horizontalDist, verticalDist) = computeViewDistances mode
  kt <- liftIO getSystemTime
  let uiAnimation =
        mkUIAnimation
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize curScreenCenter (getSize curSpace), curInfos)
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize newScreenCenter (getSize space), newInfos)
          horizontalDist verticalDist kt
          -- only when UIAnimation is over, curWorld will be replaced by newWorld.
          -- during UIAnimation, we need the two worlds.
  return $ GameState curWorld (Just newWorld) newShotNums (mkLevel newLevel) uiAnimation stAnim screen mode names

mkGameStateEssence :: WorldId -> GameState -> Maybe GameStateEssence
mkGameStateEssence wid' (GameState curWorld mayNewWorld shotNums (Level levelEssence _) _ _ _ _ _) =
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
