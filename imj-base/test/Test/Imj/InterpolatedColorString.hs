{-# LANGUAGE OverloadedStrings #-}

module Test.Imj.InterpolatedColorString(testICS) where

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad(void)

import           Data.Monoid((<>))

import           Imj.Graphics.Color
import           Imj.Graphics.Draw
import           Imj.Geo.Discrete
import           Imj.Graphics.Interpolation
import           Imj.Graphics.Text.ColorString

testICS :: (Draw e, MonadReader e m, MonadIO m)
        => m ()
testICS = do

  let from = colored "hello" (rgb 5 0 0) <> colored " world" (rgb 0 5 0) <> colored " :)" (rgb 3 5 1)
      to   = colored "hello" (rgb 5 5 5) <> colored " world" (rgb 1 2 5) <> colored " :)" (rgb 5 1 4)
      e@(Evolution _ (Frame lastFrame) _ _) = mkEvolutionEaseQuart (Successive [from, to]) 1
      from' = colored "travel" (rgb 5 0 0)
      to'   = colored "trail" (rgb 5 5 5)
      e'@(Evolution _ (Frame lastFrame') _ _) = mkEvolutionEaseQuart (Successive [from', to']) 1
      pFrom   = colored "[.]" (rgb 5 5 5)
      pTo   = colored "[......]" (rgb 5 5 5)
      e''@(Evolution _ (Frame lastFrame'') _ _) = mkEvolutionEaseQuart (Successive [pFrom, pTo]) 1
      p1   = colored "[.]" (rgb 5 5 5)
      p2   = colored "[.]" (rgb 5 0 0)
      e'''@(Evolution _ (Frame lastFrame''') _ _) = mkEvolutionEaseQuart (Successive [p1,p2,p1]) 1

  mapM_
    (\i@(Frame c') -> do
      let cs = getValueAt e i
          c = Coord c'
      drawColorString' cs (Coords (c + 10) 3) zeroCoords
    ) $ map Frame [0..lastFrame]

  mapM_
    (\i@(Frame c') -> do
      let cs = getValueAt e' i
          c = Coord c'
      drawColorString' cs (Coords (c + 10) 25) zeroCoords
    ) $ map Frame [0..lastFrame']

  mapM_
    (\i@(Frame c') -> do
      let cs = getValueAt e'' i
          c = Coord c'
      drawColorString' cs (Coords (c + 20) 25) zeroCoords
    ) $ map Frame [0..lastFrame'']

  mapM_
    (\i@(Frame c') -> do
      let cs@(ColorString l) = getValueAt e''' i
          (_,color) = head l
          c = Coord c'
      drawColorString' cs (Coords (c + 30) 25) zeroCoords
      drawStr''' (show color) (Coords (c + 30) 35) zeroCoords
    ) $ map Frame [0..lastFrame''']

drawColorString' :: (Draw e, MonadReader e m, MonadIO m)
             => ColorString
             -> Coords Pos
             -> Coords Pos
             -> m ()
drawColorString' cs pos rs =
  void (drawColorStr cs (translate pos rs))

drawStr''' :: (Draw e, MonadReader e m, MonadIO m)
         => String
         -> Coords Pos
         -> Coords Pos
         -> m (Coords Pos)
drawStr''' cs pos rs =
  drawStr'' cs (translate pos rs) (LayeredColor black white)


drawStr'' :: (Draw e, MonadReader e m, MonadIO m)
          => String
          -> Coords Pos
          -> LayeredColor
          -> m (Coords Pos)
drawStr'' str pos color =
  drawStr str pos color >> return (translateInDir Down pos)
