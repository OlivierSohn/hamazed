{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Loop.Create
        ( mkInitialState
        , mkIntermediateState
        , initialGameState
        , initialGame
        , mkGameStateEssence
        ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Data.Map(elems)

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Space
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer


initialGame :: Maybe Size
            -> SuggestedPlayerName
            -> Server
            -> ConnectionStatus
            -> IO Game
initialGame ms suggPlayerName server connectionStatus = do
  let viewMode = initialViewMode
  initialGameState initialParameters viewMode ms >>= \s ->
    return $ Game (ClientState Ongoing Excluded) viewMode s suggPlayerName server connectionStatus mkChat

initialGameState :: WorldParameters -> ViewMode -> Maybe Size -> IO GameState
initialGameState _ mode ms =
  mkInitialState mkEmptyLevelSpec mkMinimalWorldEssence mode ms Nothing

mkInitialState :: (MonadIO m)
               => LevelSpec
               -> WorldEssence
               -> ViewMode
               -> Maybe Size
               -> Maybe GameState
               -> m GameState
mkInitialState = mkIntermediateState []

mkIntermediateState :: (MonadIO m)
                    => [Int]
                    -> LevelSpec
                    -> WorldEssence
                    -> ViewMode
                    -> Maybe Size
                    -> Maybe GameState
                    -> m GameState
mkIntermediateState newShotNums newLevel essence mode maySz mayState = do
  let screen@(Screen _ newScreenCenter) = mkScreen maySz
      newWorld@(World _ _ space _ _ _) = mkWorld essence
  kt <- liftIO getSystemTime
  liftIO $ validateScreen screen
  let (curWorld@(World _ _ curSpace _ _ _), curScreenCenter, level, shotNums) =
        maybe
        (newWorld, newScreenCenter, newLevel, [])
        (\(GameState w _ curShotNums (Level curLevel _) _ (Screen _ center)) ->
            (w, center, curLevel, curShotNums))
          mayState
      curInfos = mkInfos Normal        (elems $ getWorldShips curWorld) shotNums    level
      newInfos = mkInfos ColorAnimated (elems $ getWorldShips newWorld) newShotNums newLevel
      (horizontalDist, verticalDist) = computeViewDistances mode
      uiAnimation =
        mkUIAnimation
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize curScreenCenter (getSize curSpace), curInfos)
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize newScreenCenter (getSize space), newInfos)
          horizontalDist verticalDist kt
          -- only when UIAnimation is over, curWorld will be replaced by newWorld.
          -- during UIAnimation, we need the two worlds.
  return $ GameState curWorld (Just newWorld) newShotNums (mkLevel newLevel) uiAnimation screen


mkGameStateEssence :: GameState -> GameStateEssence
mkGameStateEssence (GameState curWorld mayNewWorld shotNums _ _ _) =
  GameStateEssence (worldToEssence $ fromMaybe curWorld mayNewWorld) shotNums

mkWorld :: WorldEssence -> World
mkWorld (WorldEssence balls ships llMat wid) =
  let space = fromListOfLists llMat
      renderedSpace = mkRenderedSpace space
  in World balls ships space renderedSpace mempty wid

worldToEssence :: World -> WorldEssence
worldToEssence (World balls ships space _ _ wid) =
  WorldEssence balls ships (toListOfLists space) wid
