{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.InterpolatedColorString(testICS) where

import Imajuscule.Prelude

import System.IO( putStrLn )

import Data.Text(unpack, length)
import Data.List(mapAccumL, length)

import Control.Monad( zipWithM_ )

import Color
import Color.Interpolation
import Game.World.Space
import Game.World.Size
import Geo.Discrete.Bresenham3
import Interpolation
import Math
import Render.Console
import Render
import Text.Animated


testICS :: IO ()
testICS = do
  let from = colored "hello" (rgb 5 0 0) <> colored " world" (rgb 0 5 0) <> colored " :)" (rgb 3 5 1)
      to   = colored "hello" (rgb 5 5 5) <> colored " world" (rgb 1 2 5) <> colored " :)" (rgb 5 1 4)
      fromC = IColor8Code $ rgb 0 1 0
      toC = IColor8Code $ rgb 5 0 0
      e@(Evolution _ _ (Frame lastFrame) _ _) = mkEvolution fromC toC 1
      e'@(Evolution _ _ (Frame lastFrame') _ _) = mkEvolution from to 1
      br = bresenhamColor8 (rgb 5 0 0) (rgb 5 5 5)

  beginFrame

  mapM_
    (\i@(Frame c) -> do
      let (IColor8Code color) = fst $ evolve e i
      renderColored (colored "hi" color) (RenderState (Coords (Row c + 4) (Col 20)))
    ) $ map Frame [0..lastFrame]

  mapM_
    (\i@(Frame c) -> do
      let cs = fst $ evolve e' i
      renderColored cs (RenderState (Coords (Row c + 4) (Col 3)))
    ) $ map Frame [0..lastFrame']

  endFrame

  return ()
