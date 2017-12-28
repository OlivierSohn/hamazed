{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Loop.Create
        ( mkInitialState
        ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)

import           Imj.Game.Hamazed.Parameters
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.InTerminal
import           Imj.Graphics.UI.Animation
import           Imj.Timing

mkInitialState :: (MonadIO m)
               => GameParameters
               -> Int
               -> Maybe GameState
               -> m (Either String GameState)
mkInitialState (GameParameters shape wallType) levelNumber mayState = do
  let numbers = [1..(3+levelNumber)] -- more and more numbers as level increases
      target = sum numbers `quot` 2
      newLevel = Level levelNumber target Nothing
      newSize = worldSizeFromLevel levelNumber shape
      newAmmo = 10
      newShotNums = []
      make ew = do
        newWorld <- mkWorld ew newSize wallType numbers newAmmo
        t <- liftIO getSystemTime
        let (curWorld, level, ammo, shotNums) =
              maybe
              (newWorld, newLevel, 0, [])
              (\(GameState _ w@(World _ (BattleShip _ curAmmo _ _) _ _ _)
                           _ curShotNums curLevel _) ->
                  (w, curLevel, curAmmo, curShotNums))
                mayState
            curInfos = mkInfos Normal ammo shotNums level
            newInfos = mkInfos ColorAnimated newAmmo newShotNums newLevel
            uiAnimation =
              mkUIAnimation
                (mkWorldContainer worldFrameColors curWorld, curInfos)
                (mkWorldContainer worldFrameColors newWorld, newInfos)
                t
            gameDeadline =
              if isFinished uiAnimation
                then
                  Just $ KeyTime t
                else
                  Nothing
        return $ Right $ GameState gameDeadline curWorld newWorld newShotNums newLevel uiAnimation
  mkInTerminal newSize >>= either (return . Left) make
