{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Loop.Draw
      ( draw
      ) where

import           Imj.Prelude hiding (intercalate)
import           Prelude(length)

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import qualified Data.Map as Map(lookup, filter, keysSet)
import qualified Data.Set as Set(toList)
import           Data.Text(pack)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Class.Positionable

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World
import           Imj.Graphics.ParticleSystem.Design.Draw
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer

-- | Draws the game content.
{-# INLINABLE draw #-}
draw :: (MonadState AppState m, Draw e, MonadReader e m, MonadIO m)
     => m ()
draw =
  gets game >>= \(Game status
                     (GameState world@(World _ _ _ renderedSpace animations _) mayFutWorld _ _ wa (Screen _ screenCenter@(Coords rowCenter _)) mode _)
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
    drawStatus screenCenter status


{-# INLINABLE drawStatus #-}
drawStatus :: (MonadState AppState m, Draw e, MonadReader e m, MonadIO m)
           => Coords Pos
           -> ClientState
           -> m ()
drawStatus ref = \case
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
    PlayLevel status -> inform' =<< statusMsg status
 where
  statusMsg = \case
    New -> return [color "Waiting for game start..."]
    CancelledNoConnectedPlayer -> return [color "Game cancelled, all players left."]
    Paused disconnectedPlayers x -> -- TODO we could draw the previous status too (stack of status)
      intercalate ", " <$> showPlayerNames disconnectedPlayers >>= \them ->
        flip (++) [color "Game paused, waiting for [" <> them <> color "] to reconnect..."]  <$> statusMsg x
    Running -> return []
    WaitingForOthersToEndLevel stillPlaying ->
      intercalate ", " <$> showPlayerNames stillPlaying >>= \them ->
        return [color "Waiting for [" <> them <> color "] to finish..."]
    Countdown n x ->
      flip (++) [colored ("(" <> pack (show n) <> ")") neutralMessageColorFg] <$> statusMsg x
    OutcomeValidated o -> return [colored' (case o of
      (Lost reason) -> "You lose (" <> reason <> ")"
      Won           -> "You win!") $ messageColor o]
    WhenAllPressedAKey x (Just _) _ -> statusMsg x
    WhenAllPressedAKey x Nothing havePressed ->
      getMyShipId >>= maybe
        (error "todo")
        (\me -> flip (++) <$> maybe
          (error "logic")
          (\iHavePressed ->
            if iHavePressed
              then
                intercalate ", " <$> showPlayerNames (Map.keysSet $ Map.filter (== False) havePressed) >>= \them ->
                  return [color "Waiting for [" <> them <> color "] to press a key..."]
              else
                return [colored "Press a key to continue..." neutralMessageColorFg])
          (Map.lookup me havePressed)
          <*> statusMsg x)
  inform' = zipWithM_
    (flip drawAligned_) (map (\i -> mkCentered $ move (2*i) Down ref) [0..])
  inform m = inform' [color m]
  color = flip colored' (messageColor Won)
  showPlayerNames = mapM showPlayerName . Set.toList
  showPlayerName x =
    getPlayer x >>= return . maybe
      (colored (pack $ show x) white)
      (\(Player (PlayerName name) _ (PlayerColors c _)) -> colored name c)

{-
{-# INLINABLE drawLevelMessage #-}
drawLevelMessage :: (Draw e, MonadReader e m, MonadIO m)
                 => Coords Pos
                 -> Level
                 -> m ()
drawLevelMessage ref (Level (LevelEssence level _ _) mayLevelState) =
  mapM_ (drawLevelState ref level) mayLevelState

{-# INLINABLE drawLevelState #-}
drawLevelState :: (Draw e, MonadReader e m, MonadIO m)
               => Coords Pos
               -> Int
               -> LevelOutcome
               -> m ()
drawLevelState centerRef level stop = do

  drawAligned_ (Colored (messageColor stop) stopMsg) (mkCentered centerRef)
  when (messageState == ContinueMessage) $
    drawAligned_ (Colored neutralMessageColor $
      if level == lastLevel
        then
          "You reached the end of the game!"
        else
          let action = case stop of
                            (Lost _) -> "restart"
                            Won      -> "continue"
          in "Press a key to " <> action <> " ..." :: Text)
           (mkCentered $ move 2 Down centerRef)
-}
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
      {-
    >>= section "Center view"
      [ "'d' : On space"
      , "'f' : On ship"
      ]
      -}
    >>= section "OpenGL only"
      [ "'Up' 'Down' : Change font"
      , "'Left' 'Right' : Change font size"
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
