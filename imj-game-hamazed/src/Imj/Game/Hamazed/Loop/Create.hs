{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Loop.Create
        ( mkInitialState
        , initialGameState
        , initialGame
        ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.IO.Class(MonadIO)

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer


initialGame :: Maybe Size -> [ShipId] -> IO Game
initialGame ms ships =
  initialGameState initialParameters ships ms
    >>= return . Game Configure initialParameters

initialGameState :: GameParameters -> [ShipId] -> Maybe Size -> IO GameState
initialGameState params ships ms =
  mkInitialState params ms firstLevel ships Nothing

mkInitialState :: (MonadIO m)
               => GameParameters
               -> Maybe Size
               -> Int
               -> [ShipId]
               -> Maybe GameState
               -> m GameState
mkInitialState (GameParameters shape wallType mode) maySz levelNumber shipIds mayState = do
  let numbers = [1..(3+levelNumber)] -- more and more numbers as level increases
      target = sum numbers `quot` 2
      newLevel = Level levelNumber target Nothing
      newSize = worldSizeFromLevel levelNumber shape
      newAmmo = 10
      newShotNums = []
      screen@(Screen _ newScreenCenter) = mkScreen maySz
      nShips = length shipIds
  (World balls _ space d) <- mkWorld newSize wallType numbers
  posSpeeds <- liftIO $ createShipPosSpeeds nShips space (map (\(Number (PosSpeed pos _) _) -> pos) balls) []
  let ships =
        map (\(sid,posSpeed@(PosSpeed pos _)) -> BattleShip sid posSpeed newAmmo Nothing (getColliding pos balls))
        $ zip shipIds posSpeeds
      newWorld = World balls ships space d
  kt <- liftIO getSystemTime
  liftIO $ validateScreen screen
  let (curWorld@(World _ _ (Space _ curSz _) _), curScreenCenter, level, ammo, shotNums) =
        maybe
        (newWorld, newScreenCenter, newLevel, replicate nShips 0, [])
        (\(GameState _ _ w@(World _ curShips _ _) _ curShotNums curLevel _ (Screen _ center)) ->
            (w, center, curLevel, map (\(BattleShip _ _ curAmmo _ _) -> curAmmo) curShips, curShotNums))
          mayState
      curInfos = mkInfos Normal ammo shotNums level
      newInfos = mkInfos ColorAnimated (replicate nShips newAmmo) newShotNums newLevel
      (horizontalDist, verticalDist) = computeViewDistances mode
      uiAnimation =
        mkUIAnimation
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize curScreenCenter curSz, curInfos)
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize newScreenCenter newSize, newInfos)
          horizontalDist verticalDist kt
  return $ GameState Nothing initalGameMultiplicator curWorld newWorld newShotNums newLevel uiAnimation screen

-- | Ships positions will not be colliding with numbers and with each other.
createShipPosSpeeds :: Int -> Space -> [Coords Pos] -> [PosSpeed] -> IO [PosSpeed]
createShipPosSpeeds 0 _ _ res = return res
createShipPosSpeeds n space obstacles cur =
  createShipPos space obstacles >>= \posSpeed@(PosSpeed pos _) ->
    createShipPosSpeeds (pred n) space (pos:obstacles) (posSpeed:cur)
