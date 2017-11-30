{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Parameters(
        GameParameters(..)
      , getGameParameters
      , WorldShape(..)
      , WallType(..)
      , RandomParameters(..)
      , Strategy(..)
      ) where

import           Imajuscule.Prelude

import           Color

import           Game.World( mkWorld, World(..), renderWorld )
import           Game.World.Frame
import           Game.World.Space( renderSpace, RandomParameters(..), Strategy(..), WallType(..) )
import           Game.World.Size( WorldSize(..), WorldShape(..), worldSizeFromLevel )

import           IO.Blocking

import           Render.Console
import           Render( move, mkEmbeddedWorld, renderAlignedTxt_
                       , Alignment(..), go, renderAlignedTxt
                       , Coords(..), Row(..), Col(..), Direction(..), EmbeddedWorld(..))


data GameParameters = GameParameters {
    _gameParamsWorldShape :: !WorldShape
  , _gameParamsWallTypes :: !WallType
}

minRandomBlockSize :: Int
minRandomBlockSize = 6 -- using 4 it once took a very long time (one minute, then I killed the process)
                       -- 6 has always been ok

initialParameters :: GameParameters
initialParameters = GameParameters Square None

getGameParameters :: IO GameParameters
getGameParameters = update initialParameters

update :: GameParameters -> IO GameParameters
update params = do
  render params
  ec <- getCharThenFlush
  either
    (\_ -> return params)
    (\c -> if c == ' '
            then
              return params
            else
              update $ updateFromChar c params)
    ec


updateFromChar :: Char -> GameParameters -> GameParameters
updateFromChar c p@(GameParameters shape wallType) =
  case c of
    '1' -> GameParameters Square wallType
    '2' -> GameParameters Rectangle2x1 wallType
    'e' -> GameParameters shape None
    'r' -> GameParameters shape Deterministic
    't' -> GameParameters shape (Random $ RandomParameters minRandomBlockSize StrictlyOneComponent)
    _ -> p

render :: GameParameters -> IO ()
render (GameParameters shape wall) = do
  let worldSize@(WorldSize (Coords (Row rs) (Col cs))) = worldSizeFromLevel 1 shape
  ew <- mkEmbeddedWorld worldSize
  case ew of
    Left err -> error err
    Right (EmbeddedWorld _ upperLeft) -> do
      beginFrame
      world@(World _ _ _ space _) <- mkWorld Nothing worldSize wall []
      _ <- renderSpace space upperLeft >>=
        \worldCoords -> do
          renderWorld world worldCoords
          let middle = move (quot cs 2) RIGHT worldCoords
              middleCenter = move (quot (rs-1) 2 ) Down middle
              middleLow    = move (rs-1)           Down middle
              leftMargin = 3
              left = move (quot (rs-1) 2 - leftMargin) LEFT middleCenter
          prevColors <- setColors configColors
          renderAlignedTxt Centered "Game configuration" (go Down middle) >>=
            renderAlignedTxt_ Centered "------------------"

          go Down <$> renderTxt "- World shape" (move 5 Up left) >>=
              renderTxt "'1' -> width = height" >>=
                renderTxt_ "'2' -> width = 2 x height"
          go Down <$> renderTxt "- World walls" left >>=
              renderTxt "'e' -> no walls" >>=
                renderTxt "'r' -> deterministic walls" >>=
                  renderTxt_ "'t' -> random walls"

          renderAlignedTxt_ Centered "Hit 'Space' to start game" $ go Up middleLow
          restoreColors prevColors
          renderWorldFrame Nothing worldSize upperLeft
      endFrame
