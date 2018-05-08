{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Draw
      ( draw
      , computeViewDistances
      ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.IO.Class(MonadIO)

import           Imj.Game.Class
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Interpolation.Evolution
import           Imj.Graphics.ParticleSystem.Design.Draw
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Screen
import           Imj.Graphics.UI.Animation
import           Imj.ServerView.Types
import           Imj.Server.Class
import           Imj.Server.Types

import           Imj.Game.Color
import           Imj.Game.Priorities
import           Imj.Game.Status
import           Imj.Graphics.Class.UIInstructions
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer

-- | Margins between the world's rectangular container and other elements
computeViewDistances :: (Length Width, Length Height)
computeViewDistances = (20, 2)

-- | Draws the game content.
{-# INLINABLE draw #-}
draw :: (GameLogic (GameLogicT e)
       , MonadState (AppState (GameLogicT e)) m
       , MonadReader e m, Draw e
       , MonadIO m)
     => m ()
draw =
  gets game >>= \(Game _ screen@(Screen _ center@(Coords rowCenter _)) (GameState mayG anim) animations _ _ _ _ _ chat) -> do
    maybe
      (return ())
      (\g -> do
        worldCorner <- drawBackground screen g
        mapM_ (\(Prioritized _ a) -> drawSystem a worldCorner) animations
        drawForeground screen worldCorner g)
      mayG
    let (_, _, _, Coords _ col) = getSideCenters $ maybe
          (mkCenteredRectContainer center defaultFrameSize)
          (getViewport To screen)
          mayG
    drawUIAnimation anim
    let chatUpperLeft =
          Coords
            (rowCenter - fromIntegral (quot (height chat) 2))
            $ col + 4 + 2
    drawAt chat chatUpperLeft
    drawStatus

{-# INLINABLE drawStatus #-}
drawStatus :: (GameLogic g
             , MonadState (AppState g) m
             , MonadReader e m, Draw e
             , MonadIO m)
           => m ()
drawStatus =
  gets game >>= \(Game state screen (GameState g _) _ dcs _ _ (ServerView _ (ServerContent _ worldParams)) _ _) -> do
    case state of
      ClientState Ongoing (Included Setup) ->
        fmapM (drawSetup worldParams . getViewport To screen) g -- TODO using progressivelyInform
      _ ->
        return ()
    forM_ dcs $ \(_,AnimatedLine record frame _) -> drawMorphingAt record frame

{-# INLINABLE drawSetup #-}
drawSetup :: (ServerClientHandler s
            , MonadReader e m, Draw e
            , MonadIO m)
          => Maybe (ValuesT s)
          -> RectContainer
          -> m ()
drawSetup mayWorldParams cont = do
  let (topMiddle@(Coords _ c), bottomCenter, Coords r _, _) =
        getSideCenters cont
      left = move 12 LEFT (Coords r c)
  dTextAl "Game configuration" (mkCentered $ move 2 Down topMiddle)
    >>= dTextAl_ "------------------"
  dTextAl_ "Hit 'Space' to start game" (mkCentered $ move 2 Up bottomCenter)

  let initialPos = move 5 Up left

  void $ return initialPos
    >>= maybe return drawInstructions mayWorldParams
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

  drawInstructions li pos =
    foldM
      (\p (ConfigUI title i) -> case i of
        Choice l ->
          section title l p
        Continuous slider ->
          section title [] p >>= drawSlider slider
        Discrete slider ->
          section title [] p >>= drawSlider slider)
      pos
      $ instructions configColors li

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
