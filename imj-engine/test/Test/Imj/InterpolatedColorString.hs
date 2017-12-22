{-# LANGUAGE OverloadedStrings #-}

module Test.Imj.InterpolatedColorString(testICS) where

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad(void)

import           Data.Monoid((<>))

import           Imj.Color
import           Imj.Draw
import           Imj.Evolution
import           Imj.Geo.Discrete
import           Imj.Text.ColorString

testICS :: (Draw e, MonadReader e m, MonadIO m)
        => m ()
testICS = do

  let from = colored "hello" (rgb 5 0 0) <> colored " world" (rgb 0 5 0) <> colored " :)" (rgb 3 5 1)
      to   = colored "hello" (rgb 5 5 5) <> colored " world" (rgb 1 2 5) <> colored " :)" (rgb 5 1 4)
      e@(Evolution _ (Frame lastFrame) _ _) = mkEvolution (Successive [from, to]) 1
      from' = colored "travel" (rgb 5 0 0)
      to'   = colored "trail" (rgb 5 5 5)
      e'@(Evolution _ (Frame lastFrame') _ _) = mkEvolution (Successive [from', to']) 1
      pFrom   = colored "[.]" (rgb 5 5 5)
      pTo   = colored "[......]" (rgb 5 5 5)
      e''@(Evolution _ (Frame lastFrame'') _ _) = mkEvolution (Successive [pFrom, pTo]) 1
      p1   = colored "[.]" (rgb 5 5 5)
      p2   = colored "[.]" (rgb 5 0 0)
      e'''@(Evolution _ (Frame lastFrame''') _ _) = mkEvolution (Successive [p1,p2,p1]) 1

  mapM_
    (\i@(Frame c') -> do
      let cs = getValueAt e i
          c = Coord c'
      drawColored' cs (Coords (c + 10) 3) zeroCoords
    ) $ map Frame [0..lastFrame]

  mapM_
    (\i@(Frame c') -> do
      let cs = getValueAt e' i
          c = Coord c'
      drawColored' cs (Coords (c + 10) 25) zeroCoords
    ) $ map Frame [0..lastFrame']

  mapM_
    (\i@(Frame c') -> do
      let cs = getValueAt e'' i
          c = Coord c'
      drawColored' cs (Coords (c + 20) 25) zeroCoords
    ) $ map Frame [0..lastFrame'']

  mapM_
    (\i@(Frame c') -> do
      let cs@(ColorString l) = getValueAt e''' i
          (_,color) = head l
          c = Coord c'
      drawColored' cs (Coords (c + 30) 25) zeroCoords
      drawStr''' (show color) (Coords (c + 30) 35) zeroCoords
    ) $ map Frame [0..lastFrame''']

drawColored' :: (Draw e, MonadReader e m, MonadIO m)
             => ColorString
             -> Coords
             -> Coords
             -> m ()
drawColored' cs pos rs =
  void (drawColored cs (translate pos rs))

drawStr''' :: (Draw e, MonadReader e m, MonadIO m)
         => String
         -> Coords
         -> Coords
         -> m Coords
drawStr''' cs pos rs =
  drawStr'' cs (translate pos rs) (LayeredColor black white)


drawStr'' :: (Draw e, MonadReader e m, MonadIO m)
          => String
          -> Coords
          -> LayeredColor
          -> m Coords
drawStr'' str pos color =
  drawStr str pos color >> return (translateInDir Down pos)
