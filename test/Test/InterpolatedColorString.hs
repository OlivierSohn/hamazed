{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.InterpolatedColorString(testICS) where

import Imajuscule.Prelude

import Color
import Color.Interpolation
import Game.World.Space
import Render.Console


testICS :: IO ()
testICS = do
  ctxt <- newContext
  setFrameDimensions TerminalSize ctxt
  beginFrame

  let from = colored "hello" (rgb 5 0 0) <> colored " world" (rgb 0 5 0) <> colored " :)" (rgb 3 5 1)
      to   = colored "hello" (rgb 5 5 5) <> colored " world" (rgb 1 2 5) <> colored " :)" (rgb 5 1 4)
      e@(Evolution _ (Frame lastFrame) _ _) = mkEvolution2 from to 1
      from' = colored "travel" (rgb 5 0 0)
      to'   = colored "trail" (rgb 5 5 5)
      e'@(Evolution _ (Frame lastFrame') _ _) = mkEvolution2 from' to' 1
      pFrom   = colored "[.]" (rgb 5 5 5)
      pTo   = colored "[......]" (rgb 5 5 5)
      e''@(Evolution _ (Frame lastFrame'') _ _) = mkEvolution2 pFrom pTo 1
      p1   = colored "[.]" (rgb 5 5 5)
      p2   = colored "[.]" (rgb 5 0 0)
      e'''@(Evolution _ (Frame lastFrame''') _ _) = mkEvolution (Successive [p1,p2,p1]) 1

  mapM_
    (\i@(Frame c) -> do
      let cs = evolve e i
      renderColored cs (RenderState (Coords (Row c + 10) (Col 3)) ctxt)
    ) $ map Frame [0..lastFrame]

  mapM_
    (\i@(Frame c) -> do
      let cs = evolve e' i
      renderColored cs (RenderState (Coords (Row c + 10) (Col 25)) ctxt)
    ) $ map Frame [0..lastFrame']

  mapM_
    (\i@(Frame c) -> do
      let cs = evolve e'' i
      renderColored cs (RenderState (Coords (Row c + 20) (Col 25)) ctxt)
    ) $ map Frame [0..lastFrame'']

  mapM_
    (\i@(Frame c) -> do
      let cs@(ColorString l) = evolve e''' i
          (_,color) = head l
      renderColored cs (RenderState (Coords (Row c + 30) (Col 25)) ctxt)
      drawStr (show color) (RenderState (Coords (Row c + 30) (Col 35)) ctxt)
    ) $ map Frame [0..lastFrame''']

  endFrame ctxt

  return ()
