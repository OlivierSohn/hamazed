{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Draw
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
import           Imj.Graphics.Screen
import           Imj.ServerView.Types

import           Imj.Game.Color
import           Imj.Game.Hamazed.World.Space.Draw
import           Imj.Graphics.Class.UIInstructions
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer

-- | Draws the game content.
{-# INLINABLE draw #-}
draw :: (GameLogic (GameLogicT e)
       , MonadState (AppState (GameLogicT e)) m
       , MonadReader e m, Draw e
       , MonadIO m)
     => m ()
draw = do
  (_,_,_,Coords _ col) <- getSideCenters <$> drawGame
  gets game >>= \(Game _ (Screen _ (Coords rowCenter _)) _ _ _ _ _ _ _ chat) -> do
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
  gets game >>= \(Game state screen g _ dcs _ _ (ServerView _ (ServerContent _ worldParams)) _ _) -> do
    case state of
      ClientState Ongoing Setup ->
        drawSetup worldParams $ getViewport screen g -- TODO using progressivelyInform
      _ ->
        return ()
    forM_ dcs $ \(_,AnimatedLine record frame _) -> drawMorphingAt record frame

{-# INLINABLE drawSetup #-}
drawSetup :: (Server s
            , MonadReader e m, Draw e
            , MonadIO m)
          => Maybe (ServerViewContentT s)
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
      $ instructions li

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
