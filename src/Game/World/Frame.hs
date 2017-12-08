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
  fg <- setForeground worldFrameColor
  evolveIO e frame
  restoreForeground fg
