{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Loop.Create
        ( mkInitialState
        , initialGameState
        , initialGame
        ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Data.Map(elems)

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.Chat
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
mkInitialState newLevel (WorldEssence balls ships llMat wid) mode maySz mayState = do
  let space = fromListOfLists llMat
      renderedSpace = mkRenderedSpace space
      newShotNums = []
      screen@(Screen _ newScreenCenter) = mkScreen maySz
      newWorld = World balls ships space renderedSpace mempty wid
  kt <- liftIO getSystemTime
  liftIO $ validateScreen screen
  let (curWorld@(World _ _ curSpace _ _ _), curScreenCenter, level, shotNums) =
        maybe
        (newWorld, newScreenCenter, newLevel, [])
        (\(GameState w _ curShotNums (Level curLevel _) _ (Screen _ center)) ->
            (w, center, curLevel, curShotNums))
          mayState
      nameAndAmmo (BattleShip name _ ammo _ _) = (name, ammo)
      curInfos = mkInfos Normal        (map nameAndAmmo $ elems $ getWorldShips curWorld) shotNums    level
      newInfos = mkInfos ColorAnimated (map nameAndAmmo $ elems $ getWorldShips newWorld) newShotNums newLevel
      (horizontalDist, verticalDist) = computeViewDistances mode
      uiAnimation =
        mkUIAnimation
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize curScreenCenter (getSize curSpace), curInfos)
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize newScreenCenter (getSize space), newInfos)
          horizontalDist verticalDist kt
  return $ GameState curWorld newWorld newShotNums (Level newLevel Nothing) uiAnimation screen
