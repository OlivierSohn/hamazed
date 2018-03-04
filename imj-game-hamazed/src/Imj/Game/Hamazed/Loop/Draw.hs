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
import           Prelude(length)

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
  gets game >>= \(Game status
                     (GameState world@(World _ _ _ renderedSpace animations _) mayFutWorld _ level wa (Screen _ screenCenter@(Coords rowCenter _)) mode _)
                     _ _ _ chat) -> do
    let offset = getWorldOffset mode world
        worldCorner = getWorldCorner world screenCenter offset
    -- draw the walls outside the matrix:
    fill (materialGlyph Wall) outerWallsColors
    -- draw the matrix:
    drawSpace renderedSpace worldCorner
    mapM_ (\(Prioritized _ a) -> drawSystem a worldCorner) animations
    drawWorld world worldCorner
    drawUIAnimation offset wa -- draw it after the world so that when it morphs
                              -- it goes over numbers and ship
    -- draw last so that the message is clearly visible:
    let w = fromMaybe world mayFutWorld
        offset' = getWorldOffset mode w
        (Coords _ col) = getWorldCorner w screenCenter offset'
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
  ClientState Over Excluded ->
    inform "Joining..."
  ClientState Over Setup ->
    inform "..."
  ClientState Over (PlayLevel _) ->
    inform "Please wait..."
  ClientState Ongoing s -> case s of
    Excluded ->
      inform "A game is currently running on the server, please wait..."
    Setup -> do
      (Screen _ center) <- getCurScreen
      getWorld >>= drawSetup . mkRectContainerWithCenterAndInnerSize center . getSize . getWorldSpace
    PlayLevel status -> case status of
      New ->
        inform "Waiting for game start..."
      CancelledNoConnectedPlayer ->
        inform "Game cancelled, all players left."
      Paused disconnectedPlayers _ -> -- TODO we could draw the previous status too (stack of status)
        intercalate ", " <$> showPlayerNames disconnectedPlayers >>= \them ->
          inform $ pack $ "Game paused, waiting for [" ++ them ++ "] to reconnect..."
      Running ->
        drawLevelMessage level ref
      WaitingForOthersToSendOutcome stillPlaying ->
        intercalate ", " <$> showPlayerNames stillPlaying >>= \them ->
          inform $ pack $ "Waiting for [" ++ them ++ "] to press a key..."
      OutcomeValidated outcome ->
        inform $ "The server validated the outcome:" <> pack (show outcome)
 where
  inform (msg :: Text) =
    drawAligned_ (Colored (messageColor Won) msg) $ mkCentered ref
  showPlayerNames = mapM showPlayerName . Set.toList
  showPlayerName x =
    getPlayer x >>= \n -> return $ maybe (show x) (\p -> let (PlayerName t) = getPlayerName p in show t) n

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

  void $ section "World shape"
      [ "'1' : width is height"
      , "'2' : width is 2 x height"
      ] (move 5 Up left)
    >>= section "World walls"
      [ "'e' : No walls"
      , "'r' : Random walls"
      ]
    >>= section "Center view"
      [ "'d' : On space"
      , "'f' : On ship"
      ]
    >>= section "Rendering"
      [ "'y' : Toggle numbers as square quarters (OpenGL only)"
      ]
 where
  section title elts pos = do
    dText_ ("[" <> title <> "]") pos
    foldM_ (flip dText) (translateInDir Down $ move 2 RIGHT pos) elts
    return $ move (2 + length elts) Down pos


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
