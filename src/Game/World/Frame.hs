{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Frame
    ( renderWorldFrame
    , -- Reexports
    module Game.World.Types
    ) where

import           Imajuscule.Prelude

import           Color

import           Game.World.Types

import           Render.Console


renderWorldFrame :: Evolution FrameAnimationParallel4 -> Frame -> IO ()
renderWorldFrame e frame = do
  c <- setColor Foreground worldFrameColor
  evolveIO e frame
  restoreColors c
