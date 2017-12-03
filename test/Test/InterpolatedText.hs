{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.InterpolatedText(testText) where

import Data.Text(unpack, length)
import Data.List(mapAccumL)

import Imajuscule.Prelude

import Control.Monad( zipWithM_ )

import Color
import Game.World.Space
import Game.World.Size
import Interpolation
import Math
import Render.Console
import Render
import Text.Animated


testText :: IO ()
testText = do
  let ta@(TextAnimation (ColorString str) (Evolution _ _ (Frame lastFrame) _ _)) =
        mkTextTranslation (ColorString [("hello",white)]) 1 (RenderState (Coords (Row 3) (Col 3))) (RenderState (Coords (Row 5) (Col 3)))
  beginFrame
  mapM_
    (\i@(Frame c) -> do
      let t = getAnimatedTextRenderStates ta i
          v = map (translate (Row (3*c)) (Col 0)) t
      renderAnimatedText' str v
    ) $ map Frame [0..lastFrame]
  endFrame
