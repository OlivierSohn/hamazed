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

import           Imj.Draw
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.InTerminal
import           Imj.Game.Hamazed.World.Render
import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World.Types
import           Imj.Geo.Discrete
import           Imj.Key.Blocking
import           Imj.Key.Types
import           Imj.Text.Alignment
import           Imj.Timing
import           Imj.UI.Animation


data GameParameters = GameParameters {
    _gameParamsWorldShape :: !WorldShape
  , _gameParamsWallDistrib :: !WallDistribution
}

minRandomBlockSize :: Int
minRandomBlockSize = 6 -- using 4 it once took a very long time (one minute, then I killed the process)
                       -- 6 has always been ok

initialParameters :: GameParameters
initialParameters = GameParameters Square None

-- | Displays the configuration UI showing the game creation options,
-- and returns when the player has finished chosing the options.
{-# INLINABLE getGameParameters #-}
getGameParameters :: (Draw e, MonadReader e m, MonadIO m)
                  => m GameParameters
getGameParameters = update initialParameters

{-# INLINABLE update #-}
update :: (Draw e, MonadReader e m, MonadIO m)
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
updateFromChar c p@(GameParameters shape wallType) =
  case c of
    '1' -> GameParameters Square wallType
    '2' -> GameParameters Rectangle2x1 wallType
    'e' -> GameParameters shape None
    'r' -> GameParameters shape Deterministic
    't' -> GameParameters shape (Random $ RandomParameters minRandomBlockSize StrictlyOneComponent)
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
render' :: (Draw e, MonadReader e m, MonadIO m)
        => GameParameters
        -> m ()
render' (GameParameters shape wall) = do
  let worldSize@(Size (Length rs) (Length cs)) = worldSizeFromLevel 1 shape
  mkInTerminal worldSize >>= \case
    Left err -> error err
    Right rew@(InTerminal _ ul) -> do
      world@(World _ _ space _ _) <- mkWorld rew worldSize wall [] 0
      _ <- renderSpace space ul >>=
        \worldCoords -> do
          renderWorld world
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
            >>= dText "'e' -> no walls"
              >>= dText "'r' -> deterministic walls"
                >>= dText_ "'t' -> random walls"

          t <- liftIO getSystemTime
          let infos = (mkWorldContainer worldFrameColors world, (([""],[""]),[[""],[""]]))
          renderUIAnimation $ mkUIAnimation infos infos t
      renderDrawing
