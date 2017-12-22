{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Parameters(
        GameParameters(..)
      , getGameParameters
      , WorldShape(..)
      , WallType(..)
      , RandomParameters(..)
      , Strategy(..)
      ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Game.Color
import           Imj.Game.World
import           Imj.Game.World.Evolution
import           Imj.Game.World.Space
import           Imj.Game.World.Size
import           Imj.Game.World.Embedded

import           Imj.Geo.Discrete

import           Imj.IO.Blocking
import           Imj.Timing
import           Imj.Text.Alignment

data GameParameters = GameParameters {
    _gameParamsWorldShape :: !WorldShape
  , _gameParamsWallTypes :: !WallType
}

minRandomBlockSize :: Int
minRandomBlockSize = 6 -- using 4 it once took a very long time (one minute, then I killed the process)
                       -- 6 has always been ok

initialParameters :: GameParameters
initialParameters = GameParameters Square None

{-# INLINABLE getGameParameters #-}
getGameParameters :: (Draw e, MonadReader e m, MonadIO m)
                  => m GameParameters
getGameParameters = update initialParameters

{-# INLINABLE update #-}
update :: (Draw e, MonadReader e m, MonadIO m)
       => GameParameters
       -> m GameParameters
update params =
  render' params >> do
    char <- liftIO getCharThenFlush
    either
      (\_ -> return params)
      (\c -> if c == ' '
              then
                return params
              else
                update $ updateFromChar c params)
        char

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
      -> LayeredColor
      -> Coords
      -> m Coords
dText txt color pos =
  drawTxt txt pos color >> return (translateInDir Down pos)

{-# INLINABLE dText_ #-}
dText_ :: (Draw e, MonadReader e m, MonadIO m)
       => Text
       -> LayeredColor
       -> Coords
       -> m ()
dText_ txt color pos =
  void (dText txt color pos)

{-# INLINABLE render' #-}
render' :: (Draw e, MonadReader e m, MonadIO m)
        => GameParameters
        -> m ()
render' (GameParameters shape wall) = do
  let worldSize@(Size (Length rs) (Length cs)) = worldSizeFromLevel 1 shape
  mkEmbeddedWorld worldSize >>= \case
    Left err -> error err
    Right rew@(EmbeddedWorld _ ul) -> do
      world@(World _ _ _ space _ _) <- mkWorld rew worldSize wall [] 0
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

          translateInDir Down <$> dText "- World shape" configColors (move 5 Up left)
            >>= dText "'1' -> width = height" configColors
              >>= dText_ "'2' -> width = 2 x height" configColors
          translateInDir Down <$> dText "- World walls" configColors left
            >>= dText "'e' -> no walls" configColors
              >>= dText "'r' -> deterministic walls" configColors
                >>= dText_ "'t' -> random walls" configColors

          t <- liftIO getSystemTime
          let infos = (mkFrameSpec worldFrameColors world, (([""],[""]),([""],[""])))
              worldAnimation = mkWorldAnimation infos infos t
          renderWorldAnimation worldAnimation
      renderDrawing
