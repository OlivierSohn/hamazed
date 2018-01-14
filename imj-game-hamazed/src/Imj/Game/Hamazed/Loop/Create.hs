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

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer
import           Imj.Timing


initialGame :: Maybe Size -> IO Game
initialGame ms =
  initialGameState initialParameters ms
    >>= return . Game Configure initialParameters

initialGameState :: GameParameters -> Maybe Size -> IO GameState
initialGameState params ms =
  mkInitialState params ms firstLevel Nothing >>= \case
    Left err -> error err
    Right newState -> return newState

mkInitialState :: (MonadIO m)
               => GameParameters
               -> Maybe Size
               -> Int
               -> Maybe GameState
               -> m (Either String GameState)
mkInitialState (GameParameters shape wallType mode) maySz levelNumber mayState = do
  let numbers = [1..(3+levelNumber)] -- more and more numbers as level increases
      target = sum numbers `quot` 2
      newLevel = Level levelNumber target Nothing
      newSize = worldSizeFromLevel levelNumber shape
      newAmmo = 10
      newShotNums = []
      screen@(Screen _ newScreenCenter) = mkScreen maySz
  newWorld <- mkWorld newSize wallType numbers newAmmo
  t <- liftIO getSystemTime
  liftIO $ validateScreen screen
  let (curWorld@(World _ _ (Space _ curSz _) _), curScreenCenter, level, ammo, shotNums) =
        maybe
        (newWorld, newScreenCenter, newLevel, 0, [])
        (\(GameState _ w@(World _ (BattleShip _ curAmmo _ _) _ _)
                     _ curShotNums curLevel _ (Screen _ center)) ->
            (w, center, curLevel, curAmmo, curShotNums))
          mayState
      curInfos = mkInfos Normal ammo shotNums level
      newInfos = mkInfos ColorAnimated newAmmo newShotNums newLevel
      (horizontalDist, verticalDist) = computeViewDistances mode
      uiAnimation =
        mkUIAnimation
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize curScreenCenter curSz, curInfos)
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize newScreenCenter newSize, newInfos)
          horizontalDist verticalDist t
  return $ Right $ GameState Nothing curWorld newWorld newShotNums newLevel uiAnimation screen
