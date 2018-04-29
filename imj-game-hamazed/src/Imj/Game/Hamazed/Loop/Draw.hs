{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Loop.Draw
      ( draw
      ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.IO.Class(MonadIO)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Positionable
import           Imj.ServerView.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.World.Space.Draw
import           Imj.Game.Hamazed.World
import           Imj.Graphics.ParticleSystem.Design.Draw
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer
import           Imj.Graphics.UI.Slider

-- | Draws the game content.
{-# INLINABLE draw #-}
draw :: (MonadState AppState m
       , MonadReader e m, Draw e
       , MonadIO m)
     => m ()
draw =
  gets game >>= \(Game _
                     (GameState world@(World _ _ _ renderedSpace animations _) mayFutWorld
                                _ _ wa _ (Screen _ screenCenter@(Coords rowCenter _)) mode _)
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
    drawStatus


{-# INLINABLE drawStatus #-}
drawStatus :: (MonadState AppState m
             , MonadReader e m, Draw e
             , MonadIO m)
           => m ()
drawStatus =
  gets game >>= \(Game state gs _ (ServerView _ (ServerContent _ worldParams)) _ _) -> do
    case state of
      (ClientState Ongoing Setup) ->
        getCurScreen >>= \(Screen _ center) -> getWorld >>=
          drawSetup worldParams . mkRectContainerWithCenterAndInnerSize center . getSize . getWorldSpace -- TODO using progressivelyInform
      _ -> return ()
    forM_ (getDrawnClientState gs) $ \(_,AnimatedLine record frame _) -> drawMorphingAt record frame

{-# INLINABLE drawSetup #-}
drawSetup :: (MonadReader e m, Draw e
            , MonadIO m)
          => Maybe WorldParameters
          -> RectContainer
          -> m ()
drawSetup mayWorldParams cont = do
  let (topMiddle@(Coords _ c), bottomCenter, Coords r _, _) =
        getSideCenters cont
      left = move 12 LEFT (Coords r c)
      wallsSizeSlider = maybe
        return
        (\(WorldParameters _ (WallDistribution size _)) ->
            drawSlider $ Slider size minBlockSize maxBlockSize (1 + maxBlockSize - minBlockSize) 'y' 'g' configColors Compact)
        mayWorldParams
      wallsProbaSlider = maybe
        return
        (\(WorldParameters _ (WallDistribution _ proba)) ->
            drawSlider $ Slider proba minWallProba maxWallProba nProbaSteps 'u' 'h' configColors Compact)
        mayWorldParams
  dTextAl "Game configuration" (mkCentered $ move 2 Down topMiddle)
    >>= dTextAl_ "------------------"
  dTextAl_ "Hit 'Space' to start game" (mkCentered $ move 2 Up bottomCenter)

  void $ section "World shape"
      [ "'1' : width = height"
      , "'2' : width = 2 x height"
      ] (move 5 Up left)
    >>= section "Walls size" []
    >>= wallsSizeSlider
    >>= section "Walls probability" []
    >>= wallsProbaSlider
      {-
    >>= section "Center view"
      [ "'d' : On space"
      , "'f' : On ship"
      ]
      -}
    >>= section "OpenGL rendering"
      [ "'Up' 'Down' : Change font"
      , "'Left' 'Right' : Change font size"
      ]
 where
  section title elts pos = do
    dText_ ("[" <> title <> "]") pos
    foldM_ (flip dText) (translateInDir Down $ move 2 RIGHT pos) elts
    return $ move (2 + length elts) Down pos
  drawSlider s upperLeft = do
    drawAt s $ move 2 RIGHT upperLeft
    return $ move (fromIntegral $ height s) Down upperLeft


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
