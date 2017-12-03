{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.InterpolatedColorString(testICS) where

import Imajuscule.Prelude

import System.IO( putStrLn )

import Data.Text(unpack, length)
import Data.List(mapAccumL)

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
  let from = colored "hello" $ rgb 5 0 0
      to   = colored "hello" $ rgb 5 5 5
      fromC = IColor8Code $ rgb 0 1 0
      toC = IColor8Code $ rgb 5 0 0
      e@(Evolution _ _ (Frame lastFrame) _ _) = mkEvolution fromC toC 1
  beginFrame
  mapM_
    (\i@(Frame c) -> do
      let (IColor8Code color) = fst $ evolve e i
      renderColored (colored "hi" color) (RenderState (Coords (Row c + 4) (Col 3)))
    ) $ map Frame [0..lastFrame]
  endFrame
--      anim = mkColorStringAnimation from to
  return ()

{--
mkColorStringAnimation :: ColorString -> ColorString -> Evolution ColorString
mkColorStringAnimation from to = mkEvolution from to 1
--}
