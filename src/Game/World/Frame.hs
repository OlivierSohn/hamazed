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
renderWorldFrame e@(Evolution (Successive successive) _ _ _) frame =
  case successive of
    [] -> error "not supposed to happen"
    FrameAnimationParallel4 (FrameSpec _ (RenderState _ ctxt)):_ -> do
      c <- setDrawColor Foreground worldFrameColor ctxt
      evolveIO e frame
      restoreDrawColors c ctxt
