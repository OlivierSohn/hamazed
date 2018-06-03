{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Draw
      ( draw
      , computeViewDistances
      , drawInstructions
      , Layout(..)
      ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.State.Strict(gets)
import           Data.List(foldl')

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
  dTextAl "Game configuration" (mkCentered $ move 2 Down topMiddle)
    >>= dTextAl_ "------------------"
  dTextAl_ "Hit 'Space' to start game" (mkCentered $ move 2 Up bottomCenter)

  void $ return (mkCentered $ move 5 Up $ Coords r c)
    >>= maybe return (drawInstructions Vertically Nothing) mayWorldParams
      {-
    >>= section "Center view"
      [ "'d' : On space"
      , "'f' : On ship"
      ]
      -}
    >>= section "OpenGL rendering" >>= drawAligned (List configColors
      [ "Shift + 'Up' / 'Down' : Change font"
      , "Shift + 'Left' / 'Right' : Change font size"
      ])

section :: (MonadIO m, Draw e, MonadReader e m)
        => Text -> Alignment -> m Alignment
section title =
  dTextAl ("[" <> title <> "]")

data Layout = Vertically | Horizontally

drawInstructions :: (UIInstructions s,
                    MonadReader e m, Draw e, MonadIO m)
                  => Layout
                  -> Maybe (Length Width)
                  -- ^ Width of a column. When Nothing, the max width of instructions is used.
                  -> s -> Alignment -> m Alignment
drawInstructions layout mayWidth instr al@(Alignment a (Coords yRef xRef)) =
  case layout of
    Vertically -> foldM
      (\p (ConfigUI title li) -> do
        p2 <- section title p
        p3 <- foldM (flip drawAligned) p2 li
        return $ moveAlignment 1 Down p3)
      al
      is
    Horizontally -> do
      als <- mapM
        (\(ConfigUI title li, oneAl) -> do
            p2 <- section title oneAl
            foldM (flip drawAligned) p2 li)
        $ zip is
        $ distributeHorizontally al maxWidth $ length is
      let y = foldl' max yRef $ map (_coordsY . alignmentRef) als
      return $ Alignment a $ Coords y xRef

 where

  is = instructions configColors instr

  widths = map (\(ConfigUI title li) -> fromMaybe (error "logic") $ maximumMaybe $ width title: map width li) is

  maxWidth' = fromMaybe 0 $ maximumMaybe widths
  maxWidth = fromMaybe maxWidth' mayWidth

distributeHorizontally :: Alignment -> Length Width -> Int -> [Alignment]
distributeHorizontally (Alignment al ref) (Length w) n = case al of
  Centered ->
    let totalWidth = margin + (w + margin) * n
        refLeft = move (quot totalWidth 2) LEFT ref
        firstCenter = move (quot w 2 + margin) RIGHT refLeft
    in map (\i -> Alignment Centered $ move (i * (w + margin)) RIGHT firstCenter) [0..n-1]
  RightAligned ->
    map (\i -> Alignment RightAligned $ move (i * (w + margin)) LEFT ref) $ reverse [0..n-1]
  LeftAligned ->
    map (\i -> Alignment LeftAligned $ move (i * (w + margin)) RIGHT ref) [0..n-1]

 where

  margin = 1


dTextAl :: (Draw e, MonadReader e m, MonadIO m)
        => Text -> Alignment -> m Alignment
dTextAl txt = drawAligned (Colored configColors txt)

dTextAl_ :: (Draw e, MonadReader e m, MonadIO m)
         => Text -> Alignment -> m ()
dTextAl_ a b = void $ dTextAl a b
