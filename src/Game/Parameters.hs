{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Game.Parameters(
        GameParameters(..)
      , getGameParameters
      , WorldShape(..)
      , WallType(..)
      , RandomParameters(..)
      , Strategy(..)
      ) where

import           Imajuscule.Prelude

import           Game.Color
import           Game.World
import           Game.World.Evolution
import           Game.World.Space
import           Game.World.Size
import           Game.World.Embedded

import           Geo.Discrete

import           IO.Blocking
import           Timing

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
getGameParameters :: (Draw e) => ReaderT e IO GameParameters
getGameParameters = update initialParameters

{-# INLINABLE update #-}
update :: (Draw e) => GameParameters -> ReaderT e IO GameParameters
update params =
  render params >> do
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
dText :: (Draw e) => Text -> Coords -> LayeredColor -> ReaderT e IO Coords
dText txt pos color =
  drawTxt txt pos color >> return (translateInDir Down pos)

{-# INLINABLE render #-}
render :: (Draw e) => GameParameters -> ReaderT e IO ()
render (GameParameters shape wall) = do
  let worldSize@(WorldSize (Coords (Coord rs) (Coord cs))) = worldSizeFromLevel 1 shape
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
              renderAlignedCentered = renderAlignedTxt Centered
          renderAlignedCentered "Game configuration" (translateInDir Down middle) configColors
            >>= \ pos ->
                  void (renderAlignedCentered "------------------" pos configColors)
          void (renderAlignedCentered "Hit 'Space' to start game" (translateInDir Up middleLow) configColors)

          translateInDir Down <$> dText "- World shape" (move 5 Up left) configColors
            >>= \ pos ->
              dText "'1' -> width = height" pos configColors
                >>= \pos2 ->
                  void (dText "'2' -> width = 2 x height" pos2 configColors)
          translateInDir Down <$> dText "- World walls" left configColors
            >>= \pos ->
              dText "'e' -> no walls" pos configColors >>=
                \pos2 ->
                  dText "'r' -> deterministic walls" pos2 configColors
                    >>= \pos3 ->
                      void (dText "'t' -> random walls" pos3 configColors)

          t <- liftIO getCurrentTime
          let infos = (mkFrameSpec worldFrameColors world, (([""],[""]),([""],[""])))
              worldAnimation = mkWorldAnimation infos infos t
          renderWorldAnimation worldAnimation
      flush
