{-# LANGUAGE OverloadedStrings #-}

module Test.Imj.InterpolatedColorString(testICS) where

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Monoid((<>))

import           Imj.Geo.Discrete
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Color
import           Imj.Graphics.Interpolation
import           Imj.Graphics.Render
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.Text.ColorString
import           Imj.Timing

testICS :: (Draw e, MonadReader e m, MonadIO m)
        => m ()
testICS = do
  let from = colored "hello" (rgb 5 0 0) <> colored " world" (rgb 0 5 0) <> colored " :)" (rgb 3 5 1)
      to   = colored "hello" (rgb 5 5 5) <> colored " world" (rgb 1 2 5) <> colored " :)" (rgb 5 1 4)
      dt = fromSecs 1
      e@(Evolution _ (Frame lastFrame) _ _) = mkEvolutionEaseQuart (Successive [from, to]) dt
      from' = colored "travel" (rgb 5 0 0)
      to'   = colored "trail" (rgb 5 5 5)
      e'@(Evolution _ (Frame lastFrame') _ _) = mkEvolutionEaseQuart (Successive [from', to']) dt
      pFrom   = colored "[.]" (rgb 5 5 5)
      pTo   = colored "[......]" (rgb 5 5 5)
      e''@(Evolution _ (Frame lastFrame'') _ _) = mkEvolutionEaseQuart (Successive [pFrom, pTo]) dt
      p1   = colored "[.]" (rgb 5 5 5)
      p2   = colored "[.]" (rgb 5 0 0)
      e'''@(Evolution _ (Frame lastFrame''') _ _) = mkEvolutionEaseQuart (Successive [p1,p2,p1]) dt

  mapM_
    (\i@(Frame c') -> do
      let cs = getValueAt e i
          c = Coord c'
      drawAt cs (Coords (c + 10) 3)
    ) $ map Frame [0..lastFrame]

  mapM_
    (\i@(Frame c') -> do
      let cs = getValueAt e' i
          c = Coord c'
      drawAt cs (Coords (c + 10) 25)
    ) $ map Frame [0..lastFrame']

  mapM_
    (\i@(Frame c') -> do
      let cs = getValueAt e'' i
          c = Coord c'
      drawAt cs (Coords (c + 20) 25)
    ) $ map Frame [0..lastFrame'']

  mapM_
    (\i@(Frame c') -> do
      let cs@(ColorString l) = getValueAt e''' i
          (_,color) = head l
          c = Coord c'
      drawAt cs (Coords (c + 30) 25)
      drawAt (Colored whiteOnBlack $ show color) (Coords (c + 30) 35)
    ) $ map Frame [0..lastFrame''']
