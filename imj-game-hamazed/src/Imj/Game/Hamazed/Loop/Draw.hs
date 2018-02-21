{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Loop.Draw
      ( draw
      ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import qualified Data.Set as Set(toList)
import           Data.Text(pack)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World.Space.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Level
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World
import           Imj.Geo.Discrete
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.ParticleSystem.Design.Draw
import           Imj.Graphics.Text.Alignment
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer

-- | Draws the game content.
{-# INLINABLE draw #-}
draw :: (MonadState AppState m, Draw e, MonadReader e m, MonadIO m)
     => m ()
draw =
  getGame >>= \(Game status
                     (GameState world@(World _ _ _ renderedSpace animations _) mayFutWorld _ level wa (Screen _ screenCenter@(Coords rowCenter _)) mode _)
                     _ _ _ chat) -> do
    let offset = getWorldOffset mode world
        worldCorner = getWorldCorner world screenCenter offset
    -- draw the walls outside the matrix:
    fill (materialChar Wall) outerWallsColors
    -- draw the matrix:
    drawSpace renderedSpace worldCorner
    mapM_ (\(Prioritized _ a) -> drawSystem a worldCorner) animations
    drawWorld world worldCorner
    drawUIAnimation offset wa -- draw it after the world so that when it morphs
                              -- it goes over numbers and ship
    -- draw last so that the message is clearly visible:
    let w = fromMaybe world mayFutWorld
        offset' = getWorldOffset mode w
        worldCorner'@(Coords _ col) = getWorldCorner w screenCenter offset'
        chatUpperLeft =
          Coords (rowCenter - fromIntegral (quot (height chat) 2))
            $ col + 4 + 2 + fromIntegral (getWidth (getSize $ getWorldSpace w))
    drawAt chat chatUpperLeft
    drawStatus level screenCenter status


{-# INLINABLE drawStatus #-}
drawStatus :: (MonadState AppState m, Draw e, MonadReader e m, MonadIO m)
           => Level
           -> Coords Pos
           -> ClientState
           -> m ()
drawStatus level ref = \case
  ClientState Over (PlayLevel _) ->
    inform "Please wait..."
  ClientState Over Setup ->
    inform "..."
  ClientState Over Excluded ->
    inform "Now joining!"
  ClientState Ongoing s -> case s of
    Excluded ->
      inform "A game is currently running on the server, please wait..."
    Setup -> do
      (Screen _ center) <- getCurScreen
      getWorld >>= drawSetup . mkRectContainerWithCenterAndInnerSize center . getSize . getWorldSpace
    PlayLevel status -> case status of
      New ->
        inform "Waiting for game start..."
      Paused disconnectedPlayers ->
        inform $ pack $
          "Game paused, waiting for [" ++
          intercalate ", " (map show $ Set.toList disconnectedPlayers) ++
          "] to reconnect..."
      Running ->
        drawLevelMessage level ref
 where
  inform (msg :: Text) =
    drawAligned_ (Colored (messageColor Won) msg) $ mkCentered ref

{-# INLINABLE drawSetup #-}
drawSetup :: (Draw e, MonadReader e m, MonadIO m)
          => RectContainer
          -> m ()
drawSetup cont = do
  let (topMiddle@(Coords _ c), bottomCenter, Coords r _, _) =
        getSideCenters cont
      left = move 12 LEFT (Coords r c)

  dTextAl "Game configuration" (mkCentered $ move 2 Down topMiddle)
    >>= dTextAl_ "------------------"
  dTextAl_ "Hit 'Space' to start game" (mkCentered $ move 2 Up bottomCenter)

  translateInDir Down <$> dText "* World shape" (move 5 Up left)
    >>= dText "'1' : width is height"
    >>= dText_ "'2' : width is 2 x height"
  translateInDir Down <$> dText "* World walls" left
    >>= dText "'e' : No walls"
    >>= dText "'r' : Deterministic walls"
    >>= dText "'t' : Random walls"
    >>= return . translateInDir Down
    >>= dText "* Center view on:"
    >>= dText "'d' : Space"
    >>= dText "'f' : Ship"
    >>= return . translateInDir Down
    >>= dText "* Rendering (OpenGL only):"
    >>= dText_ "'y' : Toggle numbers as square quarters"

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

dTextAl :: (Draw e, MonadReader e m, MonadIO m)
        => Text -> Alignment -> m Alignment
dTextAl txt = drawAligned (Colored configColors txt)

dTextAl_ :: (Draw e, MonadReader e m, MonadIO m)
         => Text -> Alignment -> m ()
dTextAl_ a b = void $ dTextAl a b
