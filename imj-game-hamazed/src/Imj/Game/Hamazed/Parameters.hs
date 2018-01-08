{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Parameters(
        GameParameters(..)
      , getGameParameters
      ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Draw
import           Imj.Game.Hamazed.World.InTerminal
import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World.Types
import           Imj.Geo.Discrete
import           Imj.Graphics.Text.Alignment
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer
import           Imj.Graphics.UI.RectArea
import           Imj.Input.Blocking
import           Imj.Input.Types
import           Imj.Timing


data GameParameters = GameParameters {
    _gameParamsWorldShape :: !WorldShape
  , _gameParamsWallDistrib :: !WallDistribution
  , _gameParamsViewMode :: !ViewMode
}

minRandomBlockSize :: Int
minRandomBlockSize = 6 -- using 4 it once took a very long time (one minute, then I killed the process)
                       -- 6 has always been ok

initialParameters :: GameParameters
initialParameters = GameParameters Rectangle2x1 (Random defaultRandom) CenterSpace

defaultRandom :: RandomParameters
defaultRandom = RandomParameters minRandomBlockSize StrictlyOneComponent

-- | Displays the configuration UI showing the game creation options,
-- and returns when the player has finished chosing the options.
{-# INLINABLE getGameParameters #-}
getGameParameters :: (Render e, MonadReader e m, MonadIO m)
                  => m GameParameters
getGameParameters = update initialParameters

{-# INLINABLE update #-}
update :: (Render e, MonadReader e m, MonadIO m)
       => GameParameters
       -> m GameParameters
update params = do
  render' params
  liftIO getKeyThenFlush >>= \case
    AlphaNum c ->
      if c == ' '
        then
          return params
        else
          update $ updateFromChar c params
    _ -> return params

updateFromChar :: Char -> GameParameters -> GameParameters
updateFromChar c p@(GameParameters shape wallType mode) =
  case c of
    '1' -> GameParameters Square wallType mode
    '2' -> GameParameters Rectangle2x1 wallType mode
    'e' -> GameParameters shape None mode
    'r' -> GameParameters shape Deterministic mode
    't' -> GameParameters shape (Random $ RandomParameters minRandomBlockSize StrictlyOneComponent) mode
    'd' -> GameParameters shape wallType CenterSpace
    'f' -> GameParameters shape wallType CenterShip
    _ -> p


{-# INLINABLE dText #-}
dText :: (Draw e, MonadReader e m, MonadIO m)
      => Text
      -> Coords Pos
      -> m (Coords Pos)
dText txt pos =
  drawTxt txt pos configColors >> return (translateInDir Down pos)

{-# INLINABLE dText_ #-}
dText_ :: (Draw e, MonadReader e m, MonadIO m)
       => Text
       -> Coords Pos
       -> m ()
dText_ txt pos =
  void (dText txt pos)

{-# INLINABLE render' #-}
render' :: (Render e, MonadReader e m, MonadIO m)
        => GameParameters
        -> m ()
render' (GameParameters shape wall _) = do
  let worldSize@(Size (Length rs) (Length cs)) = worldSizeFromLevel 1 shape
  -- we use CenterSpace to center the frame:
  mkInTerminal worldSize CenterSpace >>= \case
    Left err -> error err
    Right rew@(InTerminal _ _ (RectArea ul _)) -> do
      world@(World _ _ space _ (InTerminal _ _ sz)) <- mkWorld rew worldSize wall [] 0
      fill (materialChar Wall) outerWallsColors
      let worldCoords = translate' 1 1 ul
      drawSpace space worldCoords
      drawWorld world worldCoords
      let middle = move (quot cs 2) RIGHT worldCoords
          middleCenter = move (quot (rs-1) 2 ) Down middle
          middleLow    = move (rs-1)           Down middle
          leftMargin = 3
          left = move (quot (rs-1) 2 - leftMargin) LEFT middleCenter
      drawAlignedTxt "Game configuration" configColors (mkCentered $ translateInDir Down middle)
        >>= drawAlignedTxt_ "------------------" configColors
      drawAlignedTxt_ "Hit 'Space' to start game" configColors (mkCentered $ translateInDir Up middleLow)

      translateInDir Down <$> dText "- World shape" (move 5 Up left)
        >>= dText "'1' -> width = height"
        >>= dText_ "'2' -> width = 2 x height"
      translateInDir Down <$> dText "- World walls" left
        >>= dText "'e' -> No walls"
        >>= dText "'r' -> Deterministic walls"
        >>= dText "'t' -> Random walls"
        >>= return . translateInDir Down
        >>= dText "- Center view on:"
        >>= dText "'d' -> Space"
        >>= dText_ "'f' -> Ship"

      t <- liftIO getSystemTime
      let e = Successive [""]
          infos = (Colored worldFrameColors $ mkRectContainerWithTotalArea sz, ((e, e),[e,e]))
      drawUIAnimation zeroCoords $ mkUIAnimation infos infos 0 0 t
      renderToScreen
